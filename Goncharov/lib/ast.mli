[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
type binop = 
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Leq
  | Geq
  | Lt 
  | Gt
  | Neq

[@@@ocaml.text "/*"]

type name = string

(** The main type for our AST (дерева абстрактного синтаксиса) *)
type 'name t =
  | Var of 'name (** Variable [x] *)
  | Fun of 'name * 'name t (** fun **)
  | App of 'name t * 'name t (** Application [f g] *)
  | Int of int 
  | Neg of 'name t
  | Bin of binop * 'name t * 'name t
  | If of 'name t * 'name t * 'name t
  | Let of 'name * 'name t * 'name t 
  | LetRec of 'name * 'name t * 'name t 
