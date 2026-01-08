open Format
open X86_64
open Ast

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make (String)

type local_env = ident Smap.t

module StringSet = Set.Make (String)

(* Finding free variables *)

let findLocalVar b =
  let findLocalVar_stmt s =
    match s.sdesc with
    | SAffec (i, _) | SDecl (_, i, _, _) | SFun (i, _, _) ->
        StringSet.singleton i
    | SBexpr _ -> StringSet.empty
  in
  List.fold_left
    (fun set s -> StringSet.union set (findLocalVar_stmt s))
    StringSet.empty b

let rec findFree_expr e =
  match e.edesc with
  | EConst _ | EBool _ | EString _ -> StringSet.empty
  | EVar v -> StringSet.singleton v
  | EBexpr be -> findFree_bexpr be
  | EBlock b -> findFree_block b
  | ELam fb -> findFree_funbody fb
  | ECall (i, cl) ->
      let vl =
        List.fold_left
          (fun set beL ->
            List.fold_left
              (fun s be -> StringSet.union set (findFree_bexpr be))
              set beL)
          StringSet.empty cl
      in
      StringSet.union (StringSet.singleton i) vl
  | ECases (_, be, bl) ->
      let vBe = findFree_bexpr be in
      let vBl =
        List.fold_left
          (fun set (i, il, b) ->
            let linkedVars =
              match il with
              | None -> StringSet.empty
              | Some il -> StringSet.of_list il
            in
            let freeVars = findFree_block b in
            let branchSet =
              StringSet.union (StringSet.singleton i)
                (StringSet.diff freeVars linkedVars)
            in
            StringSet.union set branchSet)
          StringSet.empty bl
      in
      StringSet.union vBe vBl
  | EIf (be, b1, beBL, b2) ->
      let beC = findFree_bexpr be in
      let b1C = findFree_block b1 in
      let b2C = findFree_block b2 in
      let beBLC =
        List.fold_left
          (fun set (be, b) ->
            let beBC = StringSet.union (findFree_bexpr be) (findFree_block b) in
            StringSet.union set beBC)
          StringSet.empty beBL
      in
      StringSet.union beC (StringSet.union b1C (StringSet.union b2C beBLC))

and findFree_bexpr be =
  let e, beL = be in
  let v = findFree_expr e in
  let vL =
    List.fold_left
      (fun set (op, e) -> StringSet.union set (findFree_expr e))
      StringSet.empty beL
  in
  StringSet.union v vL

and findFree_block b =
  List.fold_left
    (fun set stmt -> StringSet.union set (findFree_stmt stmt))
    StringSet.empty b

and findFree_funbody fb =
  let pl, _, b = fb in
  let freeVars = findFree_block b in
  let linkedVars =
    List.fold_left (fun set (s, _) -> StringSet.add s set) StringSet.empty pl
  in
  let localVars = findLocalVar b in
  StringSet.diff (StringSet.diff freeVars linkedVars) localVars

and findFree_stmt s =
  match s.sdesc with
  | SFun (_, _, fb) -> findFree_funbody fb
  | SBexpr be -> findFree_bexpr be
  | SAffec (i, be) -> StringSet.remove i (findFree_bexpr be)
  | SDecl (_, i, _, be) -> StringSet.remove i (findFree_bexpr be)

(* phase 0 : explicitation des fermetures *)

type var_env = var Smap.t

let (lamEnv : (string, stmtC) Hashtbl.t) = Hashtbl.create 17
let counter = ref 0

let getUniqueName () =
  let n = !counter in
  counter := !counter + 1;
  "fun" ^ string_of_int n

let rec clos_expr env e =
  let newDesc =
    match e.edesc with
    | EConst i -> CConst i
    | EBool b -> CBool b
    | EString s -> CString s
    | EVar i -> CVar (Smap.find i env)
    | EBexpr be -> CBexpr (clos_bexpr env be)
    | EBlock b -> CBlock (clos_block env b)
    | ELam (pl, t, b) ->
        let freeVars = findFree_expr e in
        let env, vars =
          StringSet.fold
            (fun s (env, l) ->
              let v = Vclos (s, -1) in
              (Smap.add s v env, v :: l))
            freeVars (env, [])
        in
        let funName = getUniqueName () in
        let eClos = CClos (funName, vars) in
        let env, paramCL =
          List.fold_right
            (fun (i, t) (env, l) ->
              let v = Varg (i, -1) in
              (Smap.add i v env, (v, t) :: l))
            pl (env, [])
        in
        let bC = clos_block env b in
        let cLetFun =
          {
            sdescC = CLetFun (funName, paramCL, bC, -1);
            slocC = e.eloc;
            stypC = e.etyp;
          }
        in
        Hashtbl.replace lamEnv funName cLetFun;
        eClos
    | ECall (i, cl) ->
        let clC =
          List.fold_right
            (fun beL l ->
              let beLC =
                List.fold_right (fun be l -> clos_bexpr env be :: l) beL []
              in
              beLC :: l)
            cl []
        in
        CCall (i, clC)
    | ECases (t, be, bl) ->
        let beC = clos_bexpr env be in
        let blC =
          List.fold_left
            (fun l (i, il, b) ->
              let env, vars =
                match il with
                | None -> (env, [])
                | Some il ->
                    List.fold_right
                      (fun i (env, l) ->
                        let v = Vlocal (i, -1) in
                        (Smap.add i v env, v :: l))
                      il (env, [])
              in
              let bC = clos_block env b in
              (i, vars, bC) :: l)
            [] bl
        in
        CCases (t, beC, blC)
    | EIf (be, b1, beBL, b2) ->
        let beC = clos_bexpr env be in
        let b1C = clos_block env b1 in
        let b2C = clos_block env b2 in
        let beBLC =
          List.fold_right
            (fun (be, b) l -> (clos_bexpr env be, clos_block env b) :: l)
            beBL []
        in
        CIf (beC, b1C, beBLC, b2C)
  in
  { edescC = newDesc; elocC = e.eloc; etypC = e.etyp }

and clos_bexpr env be =
  let e, beL = be in
  let eC = clos_expr env e in
  let beLC =
    List.fold_right (fun (op, e) l -> (op, clos_expr env e) :: l) beL []
  in
  (eC, beLC)

and clos_block env b =
  let bC, _ =
    List.fold_left
      (fun (l, env) s ->
        let newEnv, newL = clos_stmt env s in
        (List.append newL l, newEnv))
      ([], env) b
  in
  bC

and clos_stmt ?(isGlobal = false) env s =
  let newEnv, newDescs =
    match s.sdesc with
    | SBexpr be -> (env, [ CBexpr (clos_bexpr env be, -1) ])
    | SFun (i, il, (pl, t, b)) ->
        let freeVars = findFree_stmt s in
        let env, vars =
          StringSet.fold
            (fun s (env, l) ->
              let v = Vclos (s, -1) in
              (Smap.add s v env, v :: l))
            freeVars (env, [])
        in
        let funName = getUniqueName () in
        let eClos =
          { edescC = CClos (funName, vars); elocC = s.sloc; etypC = s.styp }
        in
        let cFun = CFun (i, il, t, eClos, -1) in
        let env, paramCL =
          List.fold_right
            (fun (i, t) (env, l) ->
              let v = Varg (i, -1) in
              (Smap.add i v env, (v, t) :: l))
            pl (env, [])
        in
        let bC = clos_block env b in
        let cLetFun = CLetFun (funName, paramCL, bC, -1) in
        (env, [ cLetFun; cFun ])
    | SAffec (i, be) -> (env, [ CAffec (i, clos_bexpr env be, -1) ])
    | SDecl (iV, i, t, be) ->
        let v = if isGlobal then Vglobal i else Vlocal (i, -1) in
        let env = Smap.add i v env in
        (env, [ CDecl (iV, v, t, clos_bexpr env be, -1) ])
  in
  ( newEnv,
    List.fold_left
      (fun l d -> { sdescC = d; slocC = s.sloc; stypC = s.styp } :: l)
      [] newDescs )

let clos_prog p =
  Hashtbl.reset lamEnv;
  let pC, _ =
    List.fold_left
      (fun (l, env) s ->
        let newEnv, newL = clos_stmt ~isGlobal:true env s in
        (List.append newL l, newEnv))
      ([], Smap.empty) p
  in
  let pC = List.rev pC in
  Hashtbl.fold (fun _ cLetFun l -> cLetFun :: l) lamEnv pC

(* phase 1 : allocation des variables *)

let rec alloc_expr (env : local_env) (fpcur : int) (e : exprC) =
  match e.edescC with
  | CConst i -> ({ edescC = CConst i; elocC = e.elocC; etypC = e.etypC }, fpcur)

let alloc_stmt s = match s.sdescC with CBexpr (be, fpmax) -> failwith ""
let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr e =
  match e.edescC with
  | CConst i ->
      movq (imm 16) !%rdi
      ++ call "malloc"
      ++ movq (imm 2) (ind rax)
      ++ movq (imm i) (ind ~ofs:8 rax)
  | CCall (i, cl) -> (
      match cl with
      | [ bel ] ->
          List.fold_left
            (fun code be -> code ++ compile_bexpr be ++ pushq !%rax)
            nop bel
          ++ call i
          ++ popn (8 * List.length bel)
      | _ -> failwith "")

and compile_bexpr be =
  let e, bel = be in
  compile_expr e

and compile_stmt (codefun, codemain) s =
  match s.sdescC with
  | CBexpr (be, fpmax) ->
      let code = compile_bexpr be in
      (codefun, codemain ++ code)

let addBuiltInFunctions codefun =
  let printCode =
    label "print"
    ++ movq (imm 16) !%rdi
    ++ call "malloc"
    ++ movq (imm 2) (ind rax)
    ++ movq (imm 2) (ind ~ofs:8 rax)
  in
  codefun ++ printCode

let compile_program p ofile =
  List.iter
    (fun s ->
      let freeVars = findFree_stmt s in
      print_endline "-------";
      StringSet.iter (fun s -> print_endline s) freeVars)
    p;
  let p = clos_prog p in
  print_endline (show_fileC p);
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    {
      text =
        globl "main" ++ label "main" ++ movq !%rsp !%rbp ++ code
        ++ movq (imm 0) !%rax
        ++
        (* exit *)
        ret ++ label "print_int" ++ movq !%rdi !%rsi
        ++ movq (ilab ".Sprint_int") !%rdi
        ++ movq (imm 0) !%rax
        ++ call "printf" ++ ret ++ codefun;
      data =
        Hashtbl.fold
          (fun x _ l -> label x ++ dquad [ 1 ] ++ l)
          genv
          (label ".Sprint_int" ++ string "%d\n");
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
