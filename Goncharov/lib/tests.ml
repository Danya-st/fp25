[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

(* Property-based tests (QuickCheck) *)
open QCheck2

let binop_to_string =
  Ast.(
    function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Leq -> "<="
    | Eq -> "="
    | Geq -> ">="
    | Neq -> "<>"
    | Lt -> "<"
    | Gt -> ">")
;;


let rec to_src = function
  | Ast.Var v -> v
  | Int i -> string_of_int i
  | Neg e -> "(-" ^ to_src e ^ ")"
  | Bin (op, l, r) -> "(" ^ to_src l ^ " " ^ binop_to_string op ^ " " ^ to_src r ^ ")"
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ to_src e
  | App (f, a) -> "(" ^ to_src f ^ " " ^ to_src a ^ ")"
  | Let (x, e1, e2) -> "(let " ^ x ^ " = " ^ to_src e1 ^ " in " ^ to_src e2 ^ ")"
  | LetRec (f, e1, e2) -> "(let rec " ^ f ^ " = " ^ to_src e1 ^ " in " ^ to_src e2 ^ ")"
  | If (c, t, e) -> "(if " ^ to_src c ^ " then " ^ to_src t ^ " else " ^ to_src e ^ ")"
  | Fix -> "fix"
;;

let rec equal a b =
  match a, b with
  | Ast.Var x, Ast.Var y -> String.equal x y
  | Int x, Int y -> x = y
  | Neg x, Neg y -> equal x y
  | Fun (x1, b1), Fun (x2, b2) -> String.equal x1 x2 && equal b1 b2
  | App (f1, a1), App (f2, a2) -> equal f1 f2 && equal a1 a2
  | Bin (op1, l1, r1), Bin (op2, l2, r2) -> op1 = op2 && equal l1 l2 && equal r1 r2
  | Let (x1, e1, b1), Let (x2, e2, b2) -> String.equal x1 x2 && equal e1 e2 && equal b1 b2
  | LetRec (f1, e1, b1), LetRec (f2, e2, b2) ->
    String.equal f1 f2 && equal e1 e2 && equal b1 b2
  | If (c1, t1, e1), If (c2, t2, e2) -> equal c1 c2 && equal t1 t2 && equal e1 e2
  | Fix, Fix -> true
  | _ -> false
;;

let var_gen =
  let names = [ "x"; "y"; "z"; "n"; "m"; "f" ] in
  Gen.oneofl names
;;

let binop_gen = Gen.oneofl Ast.[ Add; Sub; Mul; Div; Leq; Eq; Geq; Lt; Gt; Neq ]
let pprint_to_string ast = Format.asprintf "%a" Pprintast.pp ast

let rec gen_ast depth =
  let open QCheck2.Gen in
  let base = 
    oneof [
      map  (fun v  -> Ast.Var v)   var_gen;
      map  (fun i  -> Ast.Int i)   (int_bound 10);   (* можно чуть увеличить *)
      return Ast.Fix;
    ] 
  in

  if depth <= 0 then base
  else
    oneof [
      base;

      (* fun только с одним аргументом *)
      map2 (fun x body -> Ast.Fun (x, body))
        var_gen
        (gen_ast (depth - 1));

      (* приложение *)
      map2 (fun f a -> Ast.App (f, a))
        (gen_ast (depth - 1))
        (gen_ast (depth - 1));

      (* бинарные операции *)
      map3 (fun op l r -> Ast.Bin (op, l, r))
        binop_gen
        (gen_ast (depth - 1))
        (gen_ast (depth - 1));

      (* let *)
      map3 (fun x e1 e2 -> Ast.Let (x, e1, e2))
        var_gen
        (gen_ast (depth - 1))
        (gen_ast (depth - 1));

      (* let rec — тоже только один аргумент в функции *)
      map4 (fun f x body e2 -> Ast.LetRec (f, Ast.Fun (x, body), e2))
        var_gen
        var_gen
        (gen_ast (depth - 1))
        (gen_ast (depth - 1));

      (* if *)
      map3 (fun c t e -> Ast.If (c, t, e))
        (gen_ast (depth - 1))
        (gen_ast (depth - 1))
        (gen_ast (depth - 1));

      (* унарный минус — добавим для полноты *)
      map (fun e -> Ast.Neg e) (gen_ast (depth - 1));
    ]

let gen_ast_sized =
  let open Gen in
  sized (fun s -> gen_ast (3 + (s mod 3)))
;;

let prop_parse_print_roundtrip =
  QCheck2.Test.make
    ~count:200
    ~name:"parser/to_src roundtrip"
    ~print:to_src
    gen_ast_sized
    (fun ast ->
       match Parser.parse (to_src ast) with
       | Result.Ok ast' -> equal ast ast'
       | Result.Error _ -> false)
;;

let prop_parse_pprint_roundtrip =
  QCheck2.Test.make
    ~count:200
    ~name:"parser/pprint roundtrip"
    ~print:pprint_to_string
    gen_ast_sized
    (fun ast ->
       match Parser.parse (pprint_to_string ast) with
       | Result.Ok ast' -> equal ast ast'
       | Result.Error _ -> false)
;;

let%test_unit "parser/to_src roundtrip" =
  QCheck2.Test.check_exn prop_parse_print_roundtrip
;;

let%test_unit "parser/pprint roundtrip" =
  QCheck2.Test.check_exn prop_parse_pprint_roundtrip
;;