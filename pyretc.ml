(* Fichier principal du compilateur pyretc *)

open Format
open Lexing
open Ast

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parseOnly = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""
let set_file f s = f := s

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options =
  [
    ( "--parse-only",
      Arg.Set parseOnly,
      "  Pour ne faire uniquement que la phase d'analyse syntaxique" );
    ( "-o",
      Arg.String (set_file ofile),
      "<file>  Pour indiquer le mom du fichier de sortie" );
  ]

let usage = "usage: pyretc [option] file.arr"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c - 1) c

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile = "" then (
    eprintf "Aucun fichier à compiler\n@?";
    exit 1);

  (* Ce fichier doit avoir l'extension .exp *)
  if not (Filename.check_suffix !ifile ".arr") then (
    eprintf "Le fichier d'entrée doit avoir l'extension .arr\n@?";
    Arg.usage options usage;
    exit 1);

  (* Par défaut, le fichier cible a le même nom que le fichier source,
     seule l'extension change *)
  if !ofile = "" then ofile := Filename.chop_suffix !ifile ".arr" ^ ".s";

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir
       le prochain token. *)
    let p = Parser.file Lexer.next_token buf in
    close_in f;

    print_endline (show_file p);

    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parseOnly then exit 0;

    assert false
    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine
       résultant de cette transformation doit être écrit dans le fichier
       cible ofile. *)
    (*Compile.compile_program p !ofile*)
  with
  | Lexer.Lexing_error s ->
      (* Erreur lexicale. On récupère sa position absolue et
	   on la convertit en numéro de ligne *)
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Lexical error : %s@." s;
      exit 1
  | Parser.Error ->
      (* Erreur syntaxique. On récupère sa position absolue et on la
	   convertit en numéro de ligne *)
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Syntax error@.";
      exit 1
