[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Real monadic interpreter goes here *)
open Ast
open Pprintast

type error = UnknownVariable of string | Type_error of string | Unbound | DivisionByZero

type output = OUnit | OInt of int | OFun of string * string Ast.t | OBuiltin of string

module Interpret : sig
  val run : int -> string Ast.t -> (output, error) result
end = struct
  let ( let* ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;
  let return x = Ok x
  let fail x = Error x
  let (>>=) = (let*)

  type value =
    | VInt of int
    | VUnit
    | VClosure of string * string Ast.t * env
    | VBuiltin of (value -> (value, error) result)

  and env = (string * value) list

  let builtin_print : value =
    VBuiltin (function
      | VInt n -> 
          print_int n; 
          print_newline (); 
          return (VInt 0)
      | _ -> fail (Type_error "print expects integer"))
  ;;

  let builtin_fix : value =
    VClosure ("f",App (Fun ("x", App (Var "f", Fun ("v", App (App (Var "x", Var "x"), Var "v")))),Fun ("x", App (Var "f", Fun ("v", App (App (Var "x", Var "x"), Var "v"))))),[])
  ;;

  let initial_env : env = [
    ("print", builtin_print);
    ("fix", builtin_fix);
  ]
  
  let run (max_steps:int) (e: string Ast.t):(output, error) result =
    let rec eval steps env e = 
      if steps > max_steps then fail (UnknownVariable "steps limit exceeded")
      else (
        match e with
        | Var v -> eval_var steps env v
        | Fun (x,e) -> return (VClosure (x,e,env))
        | App (e1,e2) -> eval_app steps env e1 e2
        | Int i -> return (VInt i)
        | Neg e -> eval_neg steps env e
        | Bin (op,e1,e2) -> eval_bin steps env op e1 e2
        | If (e1,e2,e3) -> eval_if steps env e1 e2 e3
        | Let (x,e1,e2) -> eval_let steps env x e1 e2
        | LetRec (f,Fun(x,b),e2) -> eval_letrec steps env f x b e2
        | LetRec _ -> fail (Type_error "expected function on the right"))

    and eval_var steps env e =
      match env with
      |[] -> fail (Unbound)
      |(y,v) :: rest -> if y = e then return v else eval_var (steps+1) rest e
    and eval_neg steps env e = 
      let* v = eval (steps+1) env e in
      match v with
      | VInt x -> return (VInt(-x))
      | _ -> fail (Type_error "not integer in Ast.Neg")
    and eval_bin steps env op e1 e2 = 
      let* v1 = eval (steps+1) env e1 in
      let* v2 = eval (steps+1) env e2 in
      match op, v1, v2 with
      | Add, VInt a, VInt b -> return (VInt (a + b))
      | Sub, VInt a, VInt b -> return (VInt (a - b))
      | Mul, VInt a, VInt b -> return (VInt (a * b))
      | Div, VInt _, VInt 0 -> fail (DivisionByZero)
      | Div, VInt a, VInt b -> return (VInt (a / b))
      | Leq, VInt a, VInt b -> return (VInt (if a <= b then 1 else 0))
      | Eq, VInt a, VInt b -> return (VInt (if a = b then 1 else 0))
      | Geq, VInt a, VInt b -> return (VInt (if a >= b then 1 else 0))
      | Lt, VInt a, VInt b -> return (VInt (if a< b then 1 else 0))
      | Gt, VInt a, VInt b -> return (VInt (if a > b then 1 else 0))
      | Neq, VInt a, VInt b -> return (VInt (if a <> b then 1 else 0))
      | _ -> fail (Type_error "no such binary operation or operands are not integers")
    and eval_let steps env x e1 e2 = 
      let* v1 = eval (steps+1) env e1 in eval (steps+1) ((x,v1)::env) e2
    and eval_if steps env e1 e2 e3 = 
      let* v1 = eval (steps+1) env e1 in
      match v1 with
      | VInt i -> if i <> 0 then eval (steps+1) env e2 else eval (steps+1) env e3
      | _ -> fail (Type_error "condition is not integer")
    and eval_letrec steps env f x b e2 = 
      let rec env' = (f,VClosure(x,b,env'))::env in
      eval (steps+1) env' e2
    and eval_app steps env e1 e2 = 
      let* f = eval (steps+1) env e1 in
      match f with
      |VClosure(x,body,denv) -> 
        let* arg = eval (steps+1) env e2 in
        eval (steps+1) ((x,arg)::denv) body
      |VBuiltin g -> 
        let* arg = eval (steps+1) env e2 in g arg
      | _ -> fail (Type_error "error during app")
    and print = function
      | VInt i -> return (OInt i)
      | VClosure (x,e,_) -> return (OFun(x,e))
      | VBuiltin _ -> return (OBuiltin "<builtin>")
      | VUnit -> return OUnit
    in
    let* v = eval 0 initial_env e in
    print v
  ;;
end

open Interpret

let parse_and_run ?(max_steps=10000) str =
  match Parser.parse str with
  | Ok ast ->
    (match run max_steps ast with
     | Ok n ->
       (match n with
        | OUnit -> Printf.printf "Success: Unit\n"
        | OInt n -> Printf.printf "Success: %d\n" n
        | OFun (p, b) -> Printf.printf "Success: fun %s -> %s\n" p (Format.asprintf "%a" pp b)
        | OBuiltin name -> Printf.printf "Success: <builtin: %s>\n" name)
     | Error err ->
       (match err with
        | UnknownVariable x -> Printf.eprintf "Interpreter error: Unknown variable: %s\n%!" x
        | Type_error msg -> Printf.eprintf "Interpreter error: Type error: %s\n%!" msg
        | DivisionByZero -> Printf.eprintf "Interpreter error: Division by zero\n%!"
        | Unbound -> Printf.eprintf "Interpreter error: Unbound variable\n%!" ))
  | Error _ ->
    Printf.eprintf "Parsing error\n%!";
    exit 1
;;
