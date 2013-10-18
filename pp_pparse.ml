(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

let write_ast magic ast =
  let fn = Filename.temp_file "camlppx" "" in
  let oc = open_out_bin fn in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc;
  fn

let apply_rewriter magic fn_in ppx =
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try Misc.input_bytes ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast magic fn =
  let ic = open_in_bin fn in
  try
    let buffer = Misc.input_bytes ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let apply_rewriters magic ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      let fn =
        List.fold_left (apply_rewriter magic) (write_ast magic ast) ppxs in
      read_ast magic fn

(* Parse a file *)

module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make(Camlp4.PreCast.Syntax.Ast)

module CleanAst = Camlp4.Struct.CleanAst.Make(Camlp4.PreCast.Syntax.Ast)

let print_warning = eprintf "%a:\n%s@." Camlp4.PreCast.Syntax.Loc.print

(* Camlp4 parsing *)

let init_camlp4 = lazy (
  Camlp4.Register.iter_and_take_callbacks
    (fun (name, callback)-> callback ())
)

let interface_file inputfile =
  Lazy.force init_camlp4;
  let ic = open_in_bin inputfile in
  let cs = Stream.of_channel ic in
  let loc = Camlp4.PreCast.Syntax.Loc.mk inputfile in
  Camlp4.PreCast.Syntax.current_warning := print_warning;
  let ast = 
    try 
      Camlp4.Register.CurrentParser.parse_interf loc cs
    with x -> close_in ic; raise x
  in
  close_in ic;
  let ast = Camlp4.PreCast.AstFilters.fold_interf_filters (fun t filter -> filter t) ast in
  let ast = (new CleanAst.clean_ast)#sig_item ast in
  let ast : Parsetree.signature = Obj.magic (Ast2pt.sig_item ast) in
    apply_rewriters Config.ast_intf_magic_number ast

let implementation_file inputfile =
  Lazy.force init_camlp4;
  let ic = open_in_bin inputfile in
  let cs = Stream.of_channel ic in
  let loc = Camlp4.PreCast.Syntax.Loc.mk inputfile in
  Camlp4.PreCast.Syntax.current_warning := print_warning;
  let ast = 
    try 
      Camlp4.Register.CurrentParser.parse_implem loc cs
    with 
      Camlp4.PreCast.Syntax.Loc.Exc_located(loc, exc) -> 
        close_in ic; 
        raise exc
    | x ->
        close_in ic; 
        raise x
  in
  close_in ic;
  let ast = Camlp4.PreCast.AstFilters.fold_implem_filters (fun t filter -> filter t) ast in
  let ast = (new CleanAst.clean_ast)#str_item ast in
  let ast : Parsetree.structure = Obj.magic (Ast2pt.str_item ast) in
    apply_rewriters Config.ast_impl_magic_number ast

let report_error ppf = function
  | CannotRun cmd ->
      fprintf ppf "Error while running external preprocessor@.\
                   Command line: %s@." cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd
