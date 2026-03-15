[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)
open Ast
open Utils

type error = [ `UnknownVariable of string | `Type_error of string | `Unbound | `DivisionByZero]


module Interpret (M : MONAD_FAIL) : sig
  val run : int-> 'name Ast.t -> (int, [> error ]) M.t
end = struct
    let ( let* ) m f = M.bind m ~f
  type 'name value =
    | VInt of int
    | VClosure of 'name * 'name Ast.t * 'name env
    | VFix

  and 'name env = ('name * 'name value) list
  
  let run (max_steps:int) (e: 'name Ast.t):(int,[> error]) M.t =
    let rec eval steps env e = 
      if steps > max_steps then M.fail (`UnknownVariable "steps limit exceeded")
      else (
        match e with
        | Var v -> eval_var steps env v
        | Fun (x,e) -> M.return (VClosure (x,e,env))
        | App (e1,e2) -> eval_app steps env e1 e2
        | Int i -> M.return (VInt i)
        | Neg e -> eval_neg steps env e
        | Bin (op,e1,e2) -> eval_bin steps env op e1 e2
        | If (e1,e2,e3) -> eval_if steps env e1 e2 e3
        | Let (x,e1,e2) -> eval_let steps env x e1 e2
        | Fix -> M.return VFix
        | LetRec (f,Fun(x,b),e2) -> eval_letrec steps env f x b e2
        | LetRec _ -> M.fail (`Type_error "expected function on the right"))
    and eval_var steps env e =
      match env with
      |[] -> M.fail (`Unbound)
      |(y,v) :: rest -> if y = e then M.return v else eval_var (steps+1) rest e
    and eval_neg steps env e = 
      let* v = eval (steps+1) env e in
      match v with
      | VInt x -> M.return (VInt(-x))
      | _ -> M.fail (`Type_error "not integer in Ast.Neg")
    and eval_bin steps env op e1 e2 = 
      let* v1 = eval (steps+1) env e1 in
      let* v2 = eval (steps+1) env e2 in
      match op, v1, v2 with
      | Add, VInt a, VInt b -> M.return (VInt (a + b))
      | Sub, VInt a, VInt b -> M.return (VInt (a - b))
      | Mul, VInt a, VInt b -> M.return (VInt (a * b))
      | Div, VInt _, VInt 0 -> M.fail (`DivisionByZero)
      | Div, VInt a, VInt b -> M.return (VInt (a / b))
      | Leq, VInt a, VInt b -> M.return (VInt (if a <= b then 1 else 0))
      | Eq, VInt a, VInt b -> M.return (VInt (if a = b then 1 else 0))
      | Geq, VInt a, VInt b -> M.return (VInt (if a >= b then 1 else 0))
      | Lt, VInt a, VInt b -> M.return (VInt (if a< b then 1 else 0))
      | Gt, VInt a, VInt b -> M.return (VInt (if a > b then 1 else 0))
      | Neq, VInt a, VInt b -> M.return (VInt (if a <> b then 1 else 0))
      | _ -> M.fail (`Type_error "no such binary operation or operands are not integers")
    and eval_let steps env x e1 e2 = 
      let* v1 = eval (steps+1) env e1 in eval (steps+1) ((x,v1)::env) e2
    and eval_if steps env e1 e2 e3 = 
      let* v1 = eval (steps+1) env e1 in
      match v1 with
      | VInt i -> if i = 1 then eval (steps+1) env e2 else eval (steps+1) env e3
      | _ -> M.fail (`Type_error "condition is not integer")
    and eval_letrec steps env f x b e2 = 
      let rec env' = (f,VClosure(x,b,env'))::env in
      eval (steps+1) env' e2
    and eval_app steps env e1 e2 = 
      let* f = eval (steps+1) env e1 in
      match f with
      |VClosure(x,body,denv) -> 
        let* arg = eval (steps+1) env e2 in
        eval (steps+1) ((x,arg)::denv) body
      |VFix -> eval_fix (steps+1) env e2
      | _ -> M.fail (`Type_error "error during app")
    and eval_fix steps env e = 
      let* f = eval (steps+1) env e in
      match f with
      |VClosure (fx,fbody,denv) ->
        (match fbody with
        |Fun(x,body)->
          let rec self = VClosure(x,body,((fx,self)::denv)) in M.return self
        | _ -> M.fail (`Type_error "fix expects function returning function"))
      | _ -> M.fail (`Type_error "expected function")
    and int_of_value = function
      | VInt i -> M.return i
      | VClosure _ -> M.fail (`UnknownVariable "cannot represent function as integer")
      | VFix -> M.fail (`UnknownVariable "cannot represent fix as a value")
    in
    let* v = eval 0 [] e in
    int_of_value v
  ;;
end

let parse_and_run ?(max_steps=10000) str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run max_steps) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!" ;
    exit 1
  | Result.Error (#error as err) ->
    Format.eprintf "Interpreter error: %a\n%!" 
      (fun fmt -> function
        | `UnknownVariable msg -> Format.fprintf fmt "Unknown variable: %s" msg
        | `Type_error msg -> Format.fprintf fmt "Type error: %s" msg
        | `Unbound -> Format.fprintf fmt "Unbound variable"
        | `DivisionByZero -> Format.fprintf fmt "Division by zero")
      err;
    exit 1
;;
