open StdLabels
open Camlp4
open PreCast

let sexp_of_quote =
  try Syntax.Quotation.find "sexp_of" Syntax.Quotation.DynAst.expr_tag
  with Not_found ->
    failwith "pa_structural_sexp requires but cannot find the quotation expander sexp_of"
;;

let string_of_typ =
  let syntax_printer =
    let module PP = Camlp4.Printers.OCaml.Make (Syntax) in
    new PP.printer ~comments:false ()
  in
  fun typ ->
    let buffer = Buffer.create 16 in
    Format.bprintf buffer "%a%!" syntax_printer#ctyp typ;
    Buffer.contents buffer
;;

let rec list_and_tail_of_ast_list rev_el = function
  | <:expr< [ $hd$ :: $tl$ ] >> -> list_and_tail_of_ast_list (hd :: rev_el) tl
  | <:expr< [] >> -> List.rev rev_el, None
  | e -> List.rev rev_el, Some e
;;

let rec sexp_of_expr expr =
  match expr with
  | <:expr@loc< if $e1$ then $e2$ else $e3$ >> ->
    <:expr@loc< if $e1$ then $sexp_of_expr e2$ else $sexp_of_expr e3$ >>
  | <:expr@loc< ( $e2$ : $ctyp$ ) >> ->
    let e1 = sexp_of_quote loc None (string_of_typ ctyp) in
    <:expr@loc< $e1$ $e2$ >>
  | <:expr@loc< [] >> | <:expr@loc< [ $_$ :: $_$ ] >> as e ->
    let el, tl = list_and_tail_of_ast_list [] e in
    let el = List.map el ~f:sexp_of_expr in
    let tl =
      match tl with
      | None -> <:expr@loc< [] >>
      | Some e -> <:expr@loc<
          match $sexp_of_expr e$ with
          [ Sexplib.Sexp.List l -> l
          | Sexplib.Sexp.Atom _ as sexp -> [sexp] ]
        >>
    in
    sexp_of_sexp_list loc el ~tl
  | <:expr@loc< $int:i$ >> ->
    <:expr@loc< Sexplib.Conv.sexp_of_int $int:i$ >>
  | <:expr@loc< $str:s$ >> ->
    <:expr@loc< Sexplib.Conv.sexp_of_string $str:s$ >>
  | <:expr@loc< () >> ->
    <:expr@loc< Sexplib.Sexp.List [] >>
  | <:expr@loc< $uid:constr$ >> | <:expr@loc< `$constr$ >> ->
    <:expr@loc< Sexplib.Sexp.Atom $str:constr$ >>
  | <:expr@loc< $uid:constr$ $arg$ >> | <:expr@loc< `$constr$ $arg$ >> ->
    <:expr@loc< Sexplib.Sexp.List [
                        Sexplib.Sexp.Atom $str:constr$;
                        $sexp_of_expr arg$ ] >>
  | <:expr@loc< ( $tup:e$ ) >> ->
    let el = List.map (Ast.list_of_expr e []) ~f:sexp_of_expr in
    sexp_of_sexp_list loc el ~tl:<:expr@loc< [] >>
  | <:expr@loc< { $fields$ } >> ->
    let record = List.rev (convert_bindings [] fields) in
    let l =
      List.map record ~f:(fun (loc, s, sexp) ->
        <:expr@loc< Sexplib.Sexp.List [ Sexplib.Sexp.Atom $str:s$; $sexp$ ] >>)
    in
    sexp_of_sexp_list loc l ~tl:<:expr@loc< [] >>
  | e -> Loc.raise (Ast.loc_of_expr e) (Failure "Don't know how to handle this construct")

and sexp_of_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      <:expr@loc< [ $e$ :: $acc$ ] >>)
  in
  <:expr@loc< Sexplib.Sexp.List $l$ >>

and convert_bindings acc bindings =
  match bindings with
  | <:rec_binding< $bindings1$; $bindings2$ >> ->
    convert_bindings (convert_bindings acc bindings1) bindings2
  | <:rec_binding@loc< $i$ = $e$ >> ->
    (loc, convert_id i, sexp_of_expr e) :: acc
  | <:rec_binding< >> ->
    acc
  | Ast.RbAnt _ -> assert false

and convert_id = function
  | <:ident< $lid:i$ >> -> i
  | <:ident< $uid:i$ >> -> i
  | <:ident< $id1$.$id2$ >> -> convert_id id1 ^ "." ^ convert_id id2
  | _ -> assert false
;;

let structural_sexp_quote loc _loc_name_opt cnt_str =
  sexp_of_expr (Gram.parse_string Syntax.expr_quot loc cnt_str)
;;

let error_quote id loc _loc_name_opt cnt_str =
  match Gram.parse_string Syntax.expr_quot loc cnt_str with
  | <:expr< $e1$ $e2$ >> ->
    (* would be nice to have a ~here: added automatically *)
    <:expr@loc< $id loc$ $e1$ () (fun () -> $sexp_of_expr e2$) >>
  | e -> Loc.raise (Ast.loc_of_expr e)
           (Failure "Expected an application of the form: msg argument")
;;

let () =
  Quotation.add "structural_sexp" Quotation.DynAst.expr_tag structural_sexp_quote;
  Quotation.add "raise_structural_sexp" Quotation.DynAst.expr_tag
    (error_quote (fun loc -> <:expr@loc< Error.failwiths >>));
  Quotation.add "structural_error" Quotation.DynAst.expr_tag
    (error_quote (fun loc -> <:expr@loc< Error.create >>));
  Quotation.add "structural_or_error" Quotation.DynAst.expr_tag
    (error_quote (fun loc -> <:expr@loc< Or_error.error >>))
;;
