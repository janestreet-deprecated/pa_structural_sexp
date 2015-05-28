open Core.Std

TEST_UNIT "polymorphic variant, variant, list, literal" =
  let module M = struct
    type normal_blop = Blop of int with sexp_of
    type variant_blop = [ `Message of string | `Blop of int ] with sexp_of
  end in
  <:test_result<Sexp.t>>
    ~expect:(List [
      M.sexp_of_variant_blop (`Message "string");
      M.sexp_of_variant_blop (`Blop 2);
      M.sexp_of_normal_blop (Blop 2);
    ])
    <:structural_sexp< [ `Message "string"; `Blop 2; Blop 2 ] >>
;;

TEST_UNIT "record, if" =
  <:test_result<Sexp.t>>
    ~expect:(List [
      List [Atom "message"; Atom "string"];
      List [Atom "A.blop"; Atom "1";]
    ])
    <:structural_sexp< { message = "string"; A.blop = if true then 1 else `two; } >>
;;

module A = struct
  type t = int with sexp_of
end

let a : A.t = 2

TEST_UNIT "tuple, explicit types" =
  <:test_result<Sexp.t>>
    ~expect:(List [Atom "2"; Atom "1"])
    <:structural_sexp< (a : A.t), (lazy 1 : int Lazy.t) >>
;;

TEST_UNIT "constructed list" =
  let int_list = [2; 3] in
  <:test_result<Sexp.t>>
    ~expect:(List [Atom "one"; Atom "2"; Atom "3"])
    <:structural_sexp< `one :: (int_list : int list) >>

TEST_UNIT "strange case doesn't raise an exception" =
  <:test_result<Sexp.t>>
    ~expect:(List [Atom "A"; Atom "B"])
    <:structural_sexp< `A :: `B >>

module Other_quotation_expanders = struct

  let test_exn here f =
    try f ()
    with e ->
      <:test_result<string>> ~here:[here]
        (Exn.to_string e)
        ~expect:"(message ((value 2)))"

  TEST_UNIT =
    test_exn _here_ (fun () -> <:raise_structural_sexp< "message" { value = 2 } >>)
  ;;

  TEST_UNIT =
    test_exn _here_ (fun () -> Error.raise <:structural_error< "message" { value = 2 } >>)
  ;;

  TEST_UNIT =
    test_exn _here_ (fun () -> ok_exn <:structural_or_error< "message" { value = 2 } >>)
  ;;
end
