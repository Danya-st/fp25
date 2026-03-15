[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast
let rec pp fmt = function
  | Var v -> Format.fprintf fmt "%s" v
  | Fun (x,e) -> Format.fprintf fmt "(%s -> %a)" x pp e
  | App (e1,e2) -> Format.fprintf fmt "(%a %a)" pp e1 pp e2
  | Int i -> Format.fprintf fmt "%d" i
  | Neg e -> Format.fprintf fmt "(-%a)" pp e
  | Bin (op,e1,e2) -> 
      let bop = match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "="
        | Leq -> "<="
        | Geq -> ">="
        | Neq -> "<>"
        | Lt -> "<"
        | Gt -> ">"
      in
      Format.fprintf fmt "(%a %s %a)" pp e1 bop pp e2
  | If (e1,e2,e3) -> Format.fprintf fmt "(if %a then %a else %a)" pp e1 pp e2 pp e3
  | Let (x,e1,e2) -> Format.fprintf fmt "(let %s = %a in %a)" x pp e1 pp e2
  | LetRec (f,e1,e2) -> Format.fprintf fmt "(let rec %s = %a in %a)" f pp e1 pp e2
  | Fix -> Format.fprintf fmt "fix"
;;
let pp_hum = pp