open Format
open X86_64
open Ast

exception CompilerInternalError of string

module Smap = Map.Make (String)

type local_env = int Smap.t

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
              (fun s be -> StringSet.union s (findFree_bexpr be))
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
  let set, toRemove =
    List.fold_left
      (fun (set, toR) stmt ->
        let newSet, toRemove = findFree_stmt stmt in
        (StringSet.union set newSet, StringSet.union toR toRemove))
      (StringSet.empty, StringSet.empty)
      b
  in
  StringSet.diff set toRemove

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
  | SFun (_, _, fb) -> (findFree_funbody fb, StringSet.empty)
  | SBexpr be -> (findFree_bexpr be, StringSet.empty)
  | SAffec (i, be) | SDecl (_, i, _, be) ->
      (findFree_bexpr be, StringSet.singleton i)

(* phase 0 : explicitation des fermetures *)

type var_env = var Smap.t

let (lamEnv : (string, stmtC) Hashtbl.t) = Hashtbl.create 17
let counter = ref 0

let getUniqueName () =
  let n = !counter in
  counter := !counter + 1;
  "_fun" ^ string_of_int n

let defaultEnv =
  let builtInFunctionNames =
    [ "print"; "empty"; "nothing"; "num-modulo"; "link"; "raise" ]
  in
  List.fold_left
    (fun env s -> Smap.add s (Vlocal (s, -1)) env)
    Smap.empty builtInFunctionNames

let rec clos_expr env e =
  let newDesc =
    match e.edesc with
    | EConst i -> CConst i
    | EBool b -> CBool b
    | EString s -> CString s
    | EVar i -> CVar (Smap.find i env)
    | EBexpr be -> CEBexpr (clos_bexpr env be)
    | EBlock b -> CBlock (clos_block env b)
    | ELam (pl, t, b) ->
        let freeVars = findFree_expr e in
        let envWithClos, vars, _ =
          StringSet.fold
            (fun s (env, l, n) ->
              let n = n + 8 in
              let vClos = Vclos (s, n) in
              let vEnv = Smap.find s env in
              (Smap.add s vClos env, vEnv :: l, n))
            freeVars (env, [], 8)
        in
        let funName = getUniqueName () in
        let eClos = CClos (funName, vars) in
        let envWithArgsAndClos, paramCL =
          List.fold_right
            (fun (i, t) (env, l) ->
              let v = Varg (i, -1) in
              (Smap.add i v env, (v, t) :: l))
            pl (envWithClos, [])
        in
        let bC = clos_block envWithArgsAndClos b in
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

        CCall (i, Smap.find i env, clC)
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
                        match i with
                        | "_" -> (env, Vlocal (i, -1) :: l)
                        | _ ->
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

and clos_stmt env s =
  let newEnv, newDescs =
    match s.sdesc with
    | SBexpr be -> (env, [ CBexpr (clos_bexpr env be, -1) ])
    | SFun (i, il, (pl, t, b)) ->
        let freeVars, toRemove = findFree_stmt s in
        let freeVars = StringSet.diff freeVars toRemove in
        let env = Smap.add i (Vlocal (i, -1)) env in
        let envWithClos, vars, _ =
          StringSet.fold
            (fun s (env, l, n) ->
              let n = n + 8 in
              let vClos = Vclos (s, n) in
              let vEnv = Smap.find s env in
              (Smap.add s vClos env, vEnv :: l, n))
            freeVars (env, [], 8)
        in
        let funName = getUniqueName () in
        let eClos =
          { edescC = CClos (funName, vars); elocC = s.sloc; etypC = s.styp }
        in
        let cFun = CFun (i, il, t, eClos, -1) in
        let envWithVarsAndClos, paramCL =
          List.fold_right
            (fun (i, t) (env, l) ->
              let v = Varg (i, -1) in
              (Smap.add i v env, (v, t) :: l))
            pl (envWithClos, [])
        in
        let bC = clos_block envWithVarsAndClos b in
        let cLetFun = CLetFun (funName, paramCL, bC, -1) in
        (env, [ cFun; cLetFun ])
    | SAffec (i, be) ->
        let v = Vlocal (i, -1) in
        (env, [ CAffec (v, clos_bexpr env be, -1) ])
    | SDecl (iV, i, t, be) ->
        let v = Vlocal (i, -1) in
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
        let newEnv, newL = clos_stmt env s in
        (List.append newL l, newEnv))
      ([], defaultEnv) p
  in
  let pC = List.rev pC in
  Hashtbl.fold (fun _ cLetFun l -> cLetFun :: l) lamEnv pC

(* phase 1 : allocation des variables *)
let getVarName = function Vlocal (i, _) | Vclos (i, _) | Varg (i, _) -> i

let rec alloc_expr (env : local_env) (fpcur : int) (e : exprC) =
  let newDesc, fpmax =
    match e.edescC with
    | CConst i -> (CConst i, fpcur)
    | CString s -> (CString s, fpcur)
    | CBool b -> (CBool b, fpcur)
    | CEBexpr be ->
        let beA, env, fpmax = alloc_bexpr env fpcur be in
        (CEBexpr beA, fpmax)
    | CBlock b ->
        let bA, fpmax = alloc_block env fpcur b in
        (CBlock bA, fpmax)
    | CCall (i, v, cl) ->
        let clA, fpmaxCL =
          List.fold_right
            (fun beL (l, fp) ->
              let beLC, fpmaxBe =
                List.fold_right
                  (fun be (l, fp) ->
                    let beA, _, fpA = alloc_bexpr env fpcur be in
                    (beA :: l, max fp fpA))
                  beL ([], 0)
              in
              (beLC :: l, max fp fpmaxBe))
            cl ([], 0)
        in
        (CCall (i, alloc_var env v, clA), fpmaxCL)
    | CVar v -> (CVar (alloc_var env v), fpcur)
    | CClos (i, vl) ->
        let vlA =
          List.fold_left
            (fun l v ->
              let vA = alloc_var env v in
              vA :: l)
            [] vl
        in
        (CClos (i, vlA), fpcur)
    | CIf (be, b1, beBL, b2) ->
        let beA, _, fpmaxBe = alloc_bexpr env fpcur be in
        let b1A, fpmaxB1 = alloc_block env fpcur b1 in
        let b2A, fpmaxB2 = alloc_block env fpcur b2 in
        let beBLA, fpmaxBL =
          List.fold_right
            (fun (be, b) (l, fpmax) ->
              let beA, _, fpmaxBe = alloc_bexpr env fpcur be in
              let bA, fpmaxB = alloc_block env fpcur b in
              ((beA, bA) :: l, max (max fpmaxBe fpmaxB) fpmax))
            beBL ([], fpcur)
        in
        let fpmax = max fpmaxBe (max fpmaxB1 (max fpmaxB2 fpmaxBL)) in
        (CIf (beA, b1A, beBLA, b2A), fpmax)
    | CCases (t, be, bL) ->
        let beA, _, fpmaxBe = alloc_bexpr env fpcur be in
        let blA, fpmaxBl =
          List.fold_left
            (fun (l, fp) (i, vl, b) ->
              let vlA, envVl, fpmaxVl =
                List.fold_right
                  (fun v (l, env, fp) ->
                    match getVarName v with
                    | "_" -> (v :: l, env, fp)
                    | _ ->
                        let fp = fp + 8 in
                        let env = Smap.add (getVarName v) (-fp) env in
                        let vA = alloc_var env v in
                        (vA :: l, env, fp))
                  vl ([], env, fpcur)
              in
              let bA, fpmaxB = alloc_block envVl fpmaxVl b in
              ((i, vlA, bA) :: l, max fpmaxVl fpmaxB))
            ([], fpcur) bL
        in
        (CCases (t, beA, blA), max fpmaxBe fpmaxBl)
  in
  ({ edescC = newDesc; elocC = e.elocC; etypC = e.etypC }, fpmax)

and alloc_var env = function
  | Vlocal (i, _) -> Vlocal (i, Smap.find i env)
  | Vclos (i, pos) -> Vclos (i, pos)
  | Varg (i, _) -> Varg (i, Smap.find i env)

and alloc_bexpr (env : local_env) fpcur be =
  let e, beL = be in
  let eA, fpmaxE = alloc_expr env fpcur e in
  let beLA, fpmaxBeL =
    List.fold_left
      (fun (l, fp) (op, e) ->
        let eA, fpmaxE = alloc_expr env fpcur e in
        ((op, eA) :: l, max fpmaxE fp))
      ([], 0) beL
  in
  ((eA, beLA), env, max fpmaxE fpmaxBeL)

and alloc_block env fpcur b =
  let b, _, fpmax =
    List.fold_right
      (fun s (l, env, n) ->
        let newS, newEnv, newN = alloc_stmt env n s in
        (newS :: l, newEnv, max n newN))
      b ([], env, fpcur)
  in

  (List.rev b, fpmax)

and alloc_stmt (env : local_env) (fpcur : int) s =
  let newDesc, newEnv, newFpCur =
    match s.sdescC with
    | CBexpr (be, _) ->
        let beA, env, fpmax = alloc_bexpr env fpcur be in
        (CBexpr (beA, fpmax), env, max fpcur fpmax)
    | CFun (i, il, t, e, _) ->
        let fpcur = fpcur + 8 in
        let env = Smap.add i (-fpcur) env in
        let eA, fpmax = alloc_expr env fpcur e in
        (CFun (i, il, t, eA, -fpcur), env, max fpcur fpmax)
    | CLetFun (i, pl, b, _) ->
        let plA, env, _ =
          List.fold_left
            (fun (l, env, fpcur) (v, t) ->
              let fpcur = fpcur + 8 in
              let env = Smap.add (getVarName v) fpcur env in
              let vA = alloc_var env v in
              ((vA, t) :: l, env, fpcur))
            ([], env, 16) pl (* 16 because the first argument if the closure *)
        in
        let bA, fpmax = alloc_block env 0 b in
        (CLetFun (i, plA, bA, fpmax), env, fpcur)
    | CDecl (isVar, v, t, be, _) ->
        let beA, env, fpmax = alloc_bexpr env fpcur be in
        let fpcur = fpcur + 8 in
        let env = Smap.add (getVarName v) (-fpcur) env in
        let vA = alloc_var env v in
        (CDecl (isVar, vA, t, beA, max fpcur fpmax), env, max fpcur fpmax)
    | CAffec (v, be, _) ->
        let beA, env, fpmax = alloc_bexpr env fpcur be in
        let vA = alloc_var env v in
        (CAffec (vA, beA, fpmax), env, max fpcur fpmax)
  in
  ({ sdescC = newDesc; slocC = s.slocC; stypC = s.stypC }, newEnv, newFpCur)

let alloc p =
  let initialEnv = Smap.add "nothing" (-8) Smap.empty in
  let initialEnv = Smap.add "empty" (-16) initialEnv in
  let p, _, n =
    List.fold_left
      (fun (l, env, n) s ->
        let newS, newEnv, newN = alloc_stmt env n s in
        (newS :: l, newEnv, max n newN))
      ([], initialEnv, 8 * Smap.cardinal initialEnv)
      p
  in
  (List.rev p, n)

(******************************************************************************)
(* phase 2 : production de code *)

let getUniqueLabel () =
  let n = !counter in
  counter := !counter + 1;
  "_lab" ^ string_of_int n

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let getLocalVarPos = function
  | Vlocal (_, pos) -> pos
  | Vclos (i, _) | Varg (i, _) ->
      raise
        (CompilerInternalError
           ("The variable decleration " ^ i ^ " should be local."))

let rec compile_expr ?(recursivePos = -1) e =
  match e.edescC with
  | CConst i ->
      let code =
        movq (imm 16) !%rdi
        ++ call "_my_malloc"
        ++ movq (imm 2) (ind rax)
        ++ movq (imm i) (ind ~ofs:8 rax)
      in
      (nop, code)
  | CString s ->
      let code =
        let sSize, charL = (String.length s + 1, String.to_seq s) in
        movq (imm (8 + sSize)) !%rdi
        ++ call "_my_malloc"
        ++ movq (imm 3) (ind rax)
        ++ Seq.fold_lefti
             (fun code i c ->
               code ++ movb (imm (Char.code c)) (ind ~ofs:(8 + i) rax))
             nop charL
        ++ movb (imm 0) (ind ~ofs:(7 + sSize) rax)
      in
      (nop, code)
  | CBool b ->
      let code =
        let bInt = if b then 1 else 0 in
        movq (imm 16) !%rdi
        ++ call "_my_malloc"
        ++ movq (imm 1) (ind rax)
        ++ movq (imm bInt) (ind ~ofs:8 rax)
      in
      (nop, code)
  | CVar v -> (nop, compile_var v)
  | CClos (i, vl) ->
      let code =
        let envCode, _ =
          List.fold_left
            (fun (code, n) v ->
              let newCode =
                movq !%rax !%rcx
                ++ compile_var ~recursivePos v
                ++ movq !%rax (ind ~ofs:(16 + n) rcx)
                ++ movq !%rcx !%rax
              in
              (code ++ newCode, n + 8))
            (nop, 0) vl
        in
        movq (imm (16 + (8 * List.length vl))) !%rdi
        ++ call "_my_malloc" ++ pushq !%rax
        ++ movq (imm 6) (ind rax)
        ++ movq (ilab i) (ind ~ofs:8 rax)
        ++ envCode ++ popq rax
      in
      (nop, code)
  | CCall (_, v, cl) ->
      let codeF, codeM =
        List.fold_left
          (fun (codeF, codeM) bel ->
            let codeBELF, codeBELM =
              List.fold_right
                (fun be (codeF, codeM) ->
                  let codeBEF, codeBEM = compile_bexpr be in
                  ( codeF ++ codeBEF,
                    codeM ++ codeBEM ++ popq rbx ++ pushq !%rax ++ pushq !%rbx
                  ))
                bel (nop, nop)
            in
            ( codeF ++ codeBELF,
              codeM ++ pushq !%rax ++ codeBELM ++ popq rax ++ pushq !%rax
              ++ call_star (ind ~ofs:8 rax)
              ++ popn (8 * (1 + List.length bel)) ))
          (nop, compile_var v)
          cl
      in
      (codeF, codeM)
  | CEBexpr be -> compile_bexpr be
  | CBlock b -> compile_block b
  | CIf (be, b1, beBL, b2) ->
      let endLabel = getUniqueLabel () in
      let codeBEF, codeBEM = compile_bexpr be in
      let codeB1F, codeB1M = compile_block b1 in
      let falseLab = getUniqueLabel () in
      let codeF, codeM =
        ( codeBEF ++ codeB1F,
          codeBEM
          ++ cmpq (imm 1) (ind ~ofs:8 rax) (* check if bool is true *)
          ++ jne falseLab ++ codeB1M ++ jmp endLabel ++ label falseLab )
      in
      let codeBeBLF, codeBEBLM =
        List.fold_right
          (fun (be, b) (codeF, codeM) ->
            let codeBEF, codeBEM = compile_bexpr be in
            let codeBF, codeBM = compile_block b in
            let falseLab = getUniqueLabel () in
            ( codeF ++ codeBEF ++ codeBF,
              codeM ++ codeBEM
              ++ cmpq (imm 1) (ind ~ofs:8 rax) (* check if bool is true *)
              ++ jne falseLab ++ codeBM ++ jmp endLabel ++ label falseLab ))
          beBL (codeF, codeM)
      in
      let codeB2F, codeB2M = compile_block b2 in

      (codeBeBLF ++ codeF ++ codeB2F, codeBEBLM ++ codeB2M ++ label endLabel)
  | CCases (_, be, bL) ->
      let codefunBe, codemainBe = compile_bexpr be in
      let emptyLabel = getUniqueLabel () in
      let linkLabel = getUniqueLabel () in
      let endLabel = getUniqueLabel () in

      let codefunBl, codemainBl =
        List.fold_right
          (fun (i, vl, b) (codeF, codeM) ->
            let codefunB, codemainB = compile_block b in
            let code =
              match i with
              | "empty" ->
                  label emptyLabel
                  ++ cmpq (imm 4) (ind rax)
                  ++ jne linkLabel ++ codemainB ++ jmp endLabel
              | "link" ->
                  let codeV, _ =
                    List.fold_left
                      (fun (code, n) v ->
                        match getVarName v with
                        | "_" -> (code, n + 8)
                        | _ ->
                            let pos = getLocalVarPos v in
                            ( code
                              ++ movq (ind ~ofs:n rax) !%rcx
                              ++ movq !%rcx (ind ~ofs:pos rbp),
                              n + 8 ))
                      (nop, 8) vl
                  in
                  label linkLabel
                  ++ cmpq (imm 5) (ind rax)
                  ++ jne emptyLabel ++ codeV ++ codemainB ++ jmp endLabel
              | _ ->
                  raise
                    (CompilerInternalError
                       "Others branches than empty or link in cases \
                        expressions are not yet supported.")
            in
            (codeF ++ codefunB, codeM ++ code))
          bL (codefunBe, codemainBe)
      in
      (codefunBl, codemainBl ++ label endLabel)

and compile_var ?(recursivePos = -1) = function
  | Vlocal (i, pos) ->
      if recursivePos = pos then nop else movq (ind ~ofs:pos rbp) !%rax
  | Vclos (i, pos) ->
      movq (ind ~ofs:16 rbp) !%rdx ++ movq (ind ~ofs:pos rdx) !%rax
  | Varg (i, pos) -> movq (ind ~ofs:pos rbp) !%rax

and compile_bexpr be =
  let compile_binop op e1 e2 =
    let codeFunE2, codeMainE2 = compile_expr e2 in
    let code =
      let initialCode =
        match op with
        | And ->
            let isFalseLabel = getUniqueLabel () in
            pushq !%rax
            ++ cmpq (imm 0) (ind ~ofs:8 rax)
            ++ je isFalseLabel ++ codeMainE2 ++ label isFalseLabel ++ popq rbx
            ++ movq !%rax !%rdx ++ movq !%rbx !%rax ++ movq !%rdx !%rbx
        | Or ->
            let isTrueLabel = getUniqueLabel () in
            pushq !%rax
            ++ cmpq (imm 1) (ind ~ofs:8 rax)
            ++ je isTrueLabel ++ codeMainE2 ++ label isTrueLabel ++ popq rbx
            ++ movq !%rax !%rdx ++ movq !%rbx !%rax ++ movq !%rdx !%rbx
        | _ ->
            pushq !%rax ++ codeMainE2 ++ popq rbx ++ movq !%rax !%rdx
            ++ movq !%rbx !%rax ++ movq !%rdx !%rbx
      in
      initialCode
      ++
      match op with
      | Add -> (
          match e1.etypC with
          | Some Number ->
              movq (ind ~ofs:8 rbx) !%rbx
              ++ addq (ind ~ofs:8 rax) !%rbx
              ++ pushq !%rbx
              ++ movq (imm 16) !%rdi
              ++ call "_my_malloc"
              ++ movq (imm 2) (ind rax)
              ++ popq rbx
              ++ movq !%rbx (ind ~ofs:8 rax)
          | Some String ->
              movq !%rax !%rdi ++ movq !%rbx !%rsi ++ call "_concat_strings"
          | _ ->
              raise
                (CompilerInternalError
                   "+ operator should only operate on intergers or strings"))
      | Sub ->
          movq (ind ~ofs:8 rbx) !%rbx
          ++ movq (ind ~ofs:8 rax) !%rcx
          ++ subq !%rbx !%rcx ++ pushq !%rcx
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 2) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Mul ->
          movq (ind ~ofs:8 rbx) !%rbx
          ++ imulq (ind ~ofs:8 rax) !%rbx
          ++ pushq !%rbx
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 2) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Div ->
          movq !%rax !%rcx
          ++ movq (ind ~ofs:8 rbx) !%rbx
          ++ movq (ind ~ofs:8 rax) !%rax
          ++ cqto ++ idivq !%rbx ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 2) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | And ->
          movq (ind ~ofs:8 rbx) !%rbx
          ++ andq (ind ~ofs:8 rax) !%rbx
          ++ pushq !%rbx
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Or ->
          movq (ind ~ofs:8 rbx) !%rbx
          ++ orq (ind ~ofs:8 rax) !%rbx
          ++ pushq !%rbx
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Inf ->
          movq (ind ~ofs:8 rax) !%rax
          ++ cmpq (ind ~ofs:8 rbx) !%rax
          ++ setl !%al ++ movzbq !%al rax ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | InfEq ->
          movq (ind ~ofs:8 rax) !%rax
          ++ cmpq (ind ~ofs:8 rbx) !%rax
          ++ setle !%al ++ movzbq !%al rax ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Sup ->
          movq (ind ~ofs:8 rax) !%rax
          ++ cmpq (ind ~ofs:8 rbx) !%rax
          ++ setg !%al ++ movzbq !%al rax ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | SupEq ->
          movq (ind ~ofs:8 rax) !%rax
          ++ cmpq (ind ~ofs:8 rbx) !%rax
          ++ setge !%al ++ movzbq !%al rax ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Eq ->
          pushq !%rbx ++ pushq !%rax ++ call "_check_equality" ++ popn 16
          ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
      | Dif ->
          pushq !%rbx ++ pushq !%rax ++ call "_check_equality" ++ popn 16
          ++ pushq !%rax
          ++ movq (imm 16) !%rdi
          ++ call "_my_malloc"
          ++ movq (imm 1) (ind rax)
          ++ popq rcx
          ++ xorq (imm 1) !%rcx
          ++ movq !%rcx (ind ~ofs:8 rax)
    in
    (codeFunE2, code)
  in
  let e, bel = be in
  let (codefun, codemain), _ =
    List.fold_right
      (fun (op, currentE) ((codeF, codeM), previousE) ->
        let codeBinopF, codeBinopM = compile_binop op previousE currentE in
        ((codeF ++ codeBinopF, codeM ++ codeBinopM), currentE))
      bel
      (compile_expr e, e)
  in
  (codefun, codemain)

and compile_block b = List.fold_left compile_stmt (nop, nop) b

and compile_stmt (codefun, codemain) s =
  match s.sdescC with
  | CBexpr (be, fpmax) ->
      let codeF, codeM = compile_bexpr be in
      (codefun ++ codeF, codemain ++ codeM)
  | CFun (i, _, _, e, pos) ->
      let codeEF, codeEM = compile_expr ~recursivePos:pos e in
      let code = codeEM ++ movq !%rax (ind ~ofs:pos rbp) in
      (codefun ++ codeEF, codemain ++ code)
  | CLetFun (i, pl, b, fpmax) ->
      let codeBFun, codeBMain = compile_block b in
      let code =
        label i ++ pushq !%rbp ++ movq !%rsp !%rbp ++ pushn fpmax ++ codeBMain
        ++ popn fpmax ++ popq rbp ++ ret
      in
      (code ++ codefun ++ codeBFun, codemain)
  | CDecl (_, v, _, be, fpmax) | CAffec (v, be, fpmax) ->
      let pos = getLocalVarPos v in
      let codeBEF, codeBEM = compile_bexpr be in
      let code = codeBEM ++ movq !%rax (ind ~ofs:pos rbp) in
      (codefun ++ codeBEF, codemain ++ code)

let addBuildInVariablesToCode (codefun, codemain) =
  let nothingCode =
    movq (imm 8) !%rdi
    ++ call "_my_malloc"
    ++ movq (imm 0) (ind rax)
    ++ movq !%rax (ind ~ofs:(-8) rbp)
  in
  let emptyCode =
    movq (imm 8) !%rdi
    ++ call "_my_malloc"
    ++ movq (imm 4) (ind rax)
    ++ movq !%rax (ind ~ofs:(-16) rbp)
  in
  (codefun, nothingCode ++ emptyCode ++ codemain)

let addInitialFunctionToP p = BuiltIn.each :: BuiltIn.fold :: p

let addBuildInFunctionsToP p =
  let unknownPos =
    { pos_fname = "Unknown"; pos_lnum = -1; pos_bol = -1; pos_cnum = -1 }
  in

  let linkT = Ast.PType ("a", None) in
  let linkClos =
    { edescC = CClos ("_link", []); elocC = unknownPos; etypC = None }
  in
  let linkFun =
    {
      sdescC = CFun ("link", Some [ "a" ], linkT, linkClos, -1);
      slocC = unknownPos;
      stypC = None;
    }
  in

  let numModuloT = Ast.PType ("", None) in
  let numModuloClos =
    { edescC = CClos ("_num_modulo", []); elocC = unknownPos; etypC = None }
  in
  let numModuloFun =
    {
      sdescC = CFun ("num-modulo", None, numModuloT, numModuloClos, -1);
      slocC = unknownPos;
      stypC = None;
    }
  in

  let raiseT = Ast.PType ("a", None) in
  let raiseClos =
    { edescC = CClos ("_raise", []); elocC = unknownPos; etypC = None }
  in
  let raiseFun =
    {
      sdescC = CFun ("raise", Some [ "a" ], raiseT, raiseClos, -1);
      slocC = unknownPos;
      stypC = None;
    }
  in
  let printT = Ast.PType ("a", None) in
  let printClos =
    { edescC = CClos ("_print", []); elocC = unknownPos; etypC = None }
  in
  let printFun =
    {
      sdescC = CFun ("print", Some [ "a" ], printT, printClos, -1);
      slocC = unknownPos;
      stypC = None;
    }
  in

  linkFun :: numModuloFun :: raiseFun :: printFun :: p

let addBuiltInFunctionsToCode (codefun, codemain) =
  let addCheckEqualityCode (codefun, codemain) =
    let codeF =
      let eqFalseLab = getUniqueLabel () in
      let eqTrueLab = getUniqueLabel () in
      let eqBoolLab = getUniqueLabel () in
      let eqIntLab = getUniqueLabel () in
      let eqStringLab = getUniqueLabel () in
      let eqFinalLab = getUniqueLabel () in
      let isNotComparable = getUniqueLabel () in
      label "_check_equality" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ movq (ind ~ofs:16 rbp) !%rax
      ++ movq (ind ~ofs:24 rbp) !%rbx
      ++ movq (ind rax) !%rcx
      ++ cmpq (ind rbx) !%rcx
      ++ jne eqFalseLab
      ++ cmpq (imm 0) !%rcx
      ++ je eqTrueLab
      ++ cmpq (imm 1) !%rcx
      ++ je eqBoolLab
      ++ cmpq (imm 2) !%rcx
      ++ je eqIntLab
      ++ cmpq (imm 3) !%rcx
      ++ je eqStringLab
      ++ cmpq (imm 4) !%rcx
      ++ je eqTrueLab
      ++ cmpq (imm 5) !%rcx
      ++ jne isNotComparable ++ pushq !%rbx ++ pushq !%rax
      ++ pushq (ind ~ofs:8 rbx)
      ++ pushq (ind ~ofs:8 rax)
      ++ call "_check_equality" ++ popn 16
      ++ cmpq (imm 0) !%rax
      ++ popq rax ++ popq rbx ++ je eqFalseLab ++ pushq !%rbx ++ pushq !%rax
      ++ pushq (ind ~ofs:16 rbx)
      ++ pushq (ind ~ofs:16 rax)
      ++ call "_check_equality" ++ popn 16
      ++ cmpq (imm 0) !%rax
      ++ popq rax ++ popq rbx ++ je eqFalseLab ++ jmp eqTrueLab
      ++ label eqBoolLab ++ label eqIntLab
      ++ movq (ind ~ofs:8 rax) !%rax
      ++ cmpq (ind ~ofs:8 rbx) !%rax
      ++ sete !%al ++ jmp eqFinalLab ++ label isNotComparable
      ++ movq (ilab ".S_non_comparable") !%rdi
      ++ call "_my_printf"
      ++ movq (imm 1) !%rax
      ++ movq (imm 1) !%rdi
      ++ call "exit" ++ label eqStringLab
      ++ leaq (ind ~ofs:8 rax) rdi
      ++ leaq (ind ~ofs:8 rbx) rsi
      ++ call "_my_strcmp" ++ testq !%rax !%rax ++ sete !%al ++ jmp eqFinalLab
      ++ label eqTrueLab
      ++ movb (imm 1) !%al
      ++ jmp eqFinalLab ++ label eqFalseLab
      ++ movb (imm 0) !%al
      ++ label eqFinalLab ++ movzbq !%al rax ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addCheckEqualityCode (codefun, codemain) in
  let addLinkCode (codefun, codemain) =
    let codeF =
      label "_link" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ movq (imm 24) !%rdi
      ++ call "_my_malloc"
      ++ movq (imm 5) (ind rax)
      ++ movq (ind ~ofs:24 rbp) !%rdx
      ++ movq !%rdx (ind ~ofs:8 rax)
      ++ movq (ind ~ofs:32 rbp) !%rdx
      ++ movq !%rdx (ind ~ofs:16 rax)
      ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addLinkCode (codefun, codemain) in
  let addNumModuloCode (codefun, codemain) =
    let codeF =
      label "_num_modulo" ++ pushq !%rbp ++ movq !%rsp !%rbp ++ movq !%rax !%rcx
      ++ movq (ind ~ofs:24 rbp) !%rax
      ++ movq (ind ~ofs:8 rax) !%rax
      ++ movq (ind ~ofs:32 rbp) !%rbx
      ++ movq (ind ~ofs:8 rbx) !%rbx
      ++ cqto ++ idivq !%rbx ++ movq !%rdx !%rax ++ addq !%rbx !%rax ++ cqto
      ++ idivq !%rbx ++ movq !%rdx !%rax ++ label ".modulo_store" ++ pushq !%rax
      ++ movq (imm 16) !%rdi
      ++ call "_my_malloc"
      ++ movq (imm 2) (ind rax)
      ++ popq rbx
      ++ movq !%rbx (ind ~ofs:8 rax)
      ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addNumModuloCode (codefun, codemain) in
  let addRaiseCode (codefun, codemain) =
    let codeF =
      label "_raise" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ movq (ind ~ofs:24 rbp) !%rax
      ++ leaq (ind ~ofs:8 rax) rdi
      ++ call "_my_printf"
      ++ movq (imm 1) !%rax
      ++ movq (imm 1) !%rdi
      ++ call "exit"
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addRaiseCode (codefun, codemain) in
  let addPrintCode (codefun, codemain) =
    let codeF =
      label "_print" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ movq (ind ~ofs:24 rbp) !%rax
      ++ cmpq (imm 0) (ind rax)
      ++ jne "_not_nothing"
      ++ movq (ilab ".S_nothing") !%rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_not_nothing"
      ++ cmpq (imm 1) (ind rax)
      ++ jne "_not_bool"
      ++ cmpq (imm 1) (ind ~ofs:8 rax) (* check if bool is true *)
      ++ jne "_false"
      ++ movq (ilab ".S_true") !%rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_false"
      ++ movq (ilab ".S_false") !%rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_not_bool"
      ++ cmpq (imm 2) (ind rax)
      ++ jne "_not_int"
      ++ movq (ind ~ofs:8 rax) !%rdi
      ++ call "_print_int" ++ jmp "_done" ++ label "_not_int"
      ++ cmpq (imm 3) (ind rax)
      ++ jne "_not_string"
      ++ leaq (ind ~ofs:8 rax) rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_not_string"
      ++ cmpq (imm 4) (ind rax)
      ++ jne "_not_empty"
      ++ movq (ilab ".S_empty") !%rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_not_empty"
      ++ cmpq (imm 5) (ind rax)
      ++ jne "_not_link"
      ++ movq (ilab ".S_listStart") !%rdi
      ++ call "_my_printf"
      ++ pushq (ind ~ofs:24 rbp)
      ++ label "_start_list" ++ popq rax ++ pushq !%rax
      ++ pushq (ind ~ofs:8 rax)
      ++ movq (ind ~ofs:16 rbp) !%rbx
      ++ pushq !%rbx
      ++ call_star (ind ~ofs:8 rbx)
      ++ popn 16 ++ popq rax
      ++ movq (ind ~ofs:16 rax) !%rbx
      ++ cmpq (imm 4) (ind rbx)
      ++ je "_end_list" ++ pushq !%rbx
      ++ movq (ilab ".S_listMiddle") !%rdi
      ++ call "_my_printf" ++ jmp "_start_list" ++ label "_end_list"
      ++ movq (ilab ".S_listEnd") !%rdi
      ++ call "_my_printf" ++ jmp "_done" ++ label "_not_link"
      ++ cmpq (imm 6) (ind rax)
      ++ jne "_not_nothing" ++ label "_done"
      ++ movq (ind ~ofs:24 rbp) !%rax
      ++ popq rbp ++ ret
    in

    (codefun ++ codeF, codemain)
  in
  let addPrintIntCode codefun =
    let code =
      label "_print_int" ++ movq !%rdi !%rsi
      ++ movq (ilab ".S_print_int") !%rdi
      ++ movq (imm 0) !%rax
      ++ call "_my_printf" ++ ret
    in
    codefun ++ code
  in
  let codefun = addPrintIntCode codefun in
  let codefun, codemain = addPrintCode (codefun, codemain) in

  let addMyMallocCode (codefun, codemain) =
    let codeF =
      label "_my_malloc" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "malloc" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyMallocCode (codefun, codemain) in
  let addMyPrintfCode (codefun, codemain) =
    let codeF =
      label "_my_printf" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "printf" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyPrintfCode (codefun, codemain) in
  let addMyStrLenCode (codefun, codemain) =
    let codeF =
      label "_my_strlen" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "strlen" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyStrLenCode (codefun, codemain) in

  let addMyMemCpyCode (codefun, codemain) =
    let codeF =
      label "_my_memcpy" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "memcpy" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyMemCpyCode (codefun, codemain) in
  let addMyStrCpyCode (codefun, codemain) =
    let codeF =
      label "_my_strcpy" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "strcpy" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyStrCpyCode (codefun, codemain) in
  let addMyStrCmpCode (codefun, codemain) =
    let codeF =
      label "_my_strcmp" ++ pushq !%rbp ++ movq !%rsp !%rbp
      ++ andq (imm (-16)) !%rsp
      ++ call "strcmp" ++ movq !%rbp !%rsp ++ popq rbp ++ ret
    in
    (codefun ++ codeF, codemain)
  in
  let codefun, codemain = addMyStrCmpCode (codefun, codemain) in
  let addConcatStrings (codefun, codemain) =
    let code =
      label "_concat_strings" ++ pushq !%rsi ++ pushq !%rdi
      ++ leaq (ind ~ofs:8 rdi) rdi
      ++ call "_my_strlen" ++ pushq !%rax
      ++ leaq (ind ~ofs:8 rsi) rdi
      ++ call "_my_strlen" ++ popq rdi ++ pushq !%rax ++ pushq !%rdi
      ++ addq !%rax !%rdi
      ++ addq (imm 9) !%rdi
      ++ call "_my_malloc"
      ++ movq (imm 3) (ind rax)
      ++ leaq (ind ~ofs:8 rax) rdi
      ++ popq rdx ++ popq rcx ++ popq rsi ++ pushq !%rcx ++ pushq !%rdx
      ++ pushq !%rax
      ++ leaq (ind ~ofs:8 rsi) rsi
      ++ call "_my_memcpy" ++ popq rax ++ popq rdx
      ++ leaq (ind ~ofs:8 ~index:rdx rax) rdi
      ++ popq rdx ++ popq rsi ++ pushq !%rax
      ++ leaq (ind ~ofs:8 rsi) rsi
      ++ call "_my_strcpy" ++ popq rax ++ ret
    in
    (codefun ++ code, codemain)
  in
  let codefun, codemain = addConcatStrings (codefun, codemain) in
  (codefun, codemain)

let compile_program ?(verbose = false) p ofile =
  let p = addInitialFunctionToP p in
  let p = clos_prog p in
  if verbose then print_endline "CLOSURE DONE";
  let p = addBuildInFunctionsToP p in
  if verbose then print_endline (show_fileC p);
  let p, fpmax = alloc p in
  if verbose then (
    print_endline "ALLOCATION DONE";
    print_endline (show_fileC p));
  let codefun, code = addBuiltInFunctionsToCode (nop, nop) in
  let codefun, code = addBuildInVariablesToCode (codefun, code) in
  let codefun, code = List.fold_left compile_stmt (codefun, code) p in
  let p =
    {
      text =
        globl "main" ++ label "main" ++ pushq !%rbp ++ movq !%rsp !%rbp
        ++ pushn fpmax ++ code
        ++ movq (imm 0) !%rax
        ++ popn fpmax ++ popq rbp ++ ret ++ codefun;
      (* exit *)
      data =
        label ".S_print_int" ++ string "%d" ++ label ".S_nothing"
        ++ string "nothing" ++ label ".S_true" ++ string "true"
        ++ label ".S_false" ++ string "false" ++ label ".S_empty"
        ++ string "[list: ]" ++ label ".S_listStart" ++ string "[list: "
        ++ label ".S_listMiddle" ++ string ", " ++ label ".S_listEnd"
        ++ string "]" ++ label ".S_non_comparable"
        ++ string "Attempted to compare two incomparable values.\n";
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
