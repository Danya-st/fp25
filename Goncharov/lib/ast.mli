[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type binop =
  | Add (** + **)
  | Sub (** - **)
  | Mul (** * **)
  | Div (** / **)
  | Eq (** = **)
  | Leq (** <= **)
  | Geq (** >= **)
  | Lt (** < **)
  | Gt (** > **)
  | Neq (** <> **)

[@@@ocaml.text "/*"]

type name = string

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Fun of 'name * 'name t (** fun **)
  | App of 'name t * 'name t (** Application [f g] *)
  | Int of int (** Int **)
  | Neg of 'name t (** Negative int -x **)
  | Bin of binop * 'name t * 'name t (** binary operation x * y **)
  | If of 'name t * 'name t * 'name t (** if then else **)
  | Let of 'name * 'name t * 'name t (** let x = body in e **)
  | LetRec of 'name * 'name t * 'name t (** let rec x = body in e **)
