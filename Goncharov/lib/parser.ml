(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Ast
open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let spaces1 = take_while1 is_space

let token p = spaces *> p
let token1 p = spaces1 *> p


let varname =
  let* var =
    take_while1 (function
      | 'a' .. 'z' | '_' | '0' .. '9' | '\'' -> true
      | _ -> false)
  in
  match var with
  | s when s.[0] >= '0' && s.[0] <= '9' -> fail "variable starts with number"
  | s when s.[0] = '\'' -> fail "variable starts with wrong symbol"
  | "fun" | "if" | "then" | "else" | "let" | "in" | "rec" -> fail"key word instead of variable"
  | _ -> return var
;;


let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer = take_while1 is_digit >>| int_of_string

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  ; unary : dispatch -> string Ast.t Angstrom.t
  ; mul_div : dispatch -> string Ast.t Angstrom.t
  ; add_sub : dispatch -> string Ast.t Angstrom.t
  ; comp : dispatch -> string Ast.t Angstrom.t
  }

let chainl1 p op =
  let rec iter acc =
    (let* f = op
     and+ y = p in
     iter (f acc y))
    <|> return acc
  in
  let* x = p in
  iter x
;;
let mk_args args body =
  List.fold_right (fun arg acc -> Ast.Fun (arg, acc)) args body
;;

type error = Parsing_error of string 


let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ (token (char '(') *> pack.comp pack <* token (char ')')) <?> "Parentheses expected"
        ; (token (string "let" *> spaces1) *> token varname >>= fun name -> many(token varname) >>= fun args -> token (char '=') *> pack.comp pack >>= fun bind -> token (string "in") *> pack.comp pack >>| fun body -> Ast.Let(name,mk_args args bind,body))
        ; (token (string "fun"*> spaces1) *> many1(token varname) <* token (string "->") >>= fun args -> pack.comp pack >>| fun body -> (mk_args args body))
        ; ((many1(token varname) <* token (string "->") >>= fun args -> pack.comp pack >>| fun body -> (mk_args args body)))
        ; (token (string "if" *> spaces1) *> pack.comp pack >>= fun cond -> token (string "then")*> pack.comp pack>>= fun e1 -> token (string "else") *> pack.comp pack >>| fun e2 ->Ast.If(cond,e1,e2))
        ; (token (string "let") *> token1 (string "rec") *> token varname >>= fun name -> many(token varname) >>= fun args -> token (char '=') *> pack.comp pack >>= fun bind -> token (string "in") *> pack.comp pack >>| fun body -> Ast.LetRec(name,mk_args args bind,body))
        ; (token  varname >>| fun var -> Ast.Var(var))
        ; ((token integer) >>| fun num -> Ast.Int(num))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  and unary pack = 
    fix(fun _ ->
    (token (char '-')*> pack.apps pack >>| fun e ->Ast.Neg(e))
    <|> (token (char '+')*> pack.apps pack <|> pack.apps pack))
  and mul_div pack = 
    let op = 
      conde
        [(token (char '*') *> return (fun l r -> Ast.Bin(Ast.Mul,l,r)))
         ;(token (char '/') *> return (fun l r -> Ast.Bin(Ast.Div,l,r)))]
    in chainl1 (pack.unary pack) op
  and add_sub pack = 
    let op = 
      conde
        [(token (char '+') *> return (fun l r -> Ast.Bin(Ast.Add,l,r)))
         ;(token (char '-') *> return (fun l r -> Ast.Bin(Ast.Sub,l,r)))]
    in chainl1 (pack.mul_div pack) op
  and comp pack =
    let op = 
      conde
      [(token (string "<=")*> return (fun l r -> Ast.Bin(Ast.Leq,l,r)))
       ;(token (string "<>")*> return (fun l r -> Ast.Bin(Ast.Neq,l,r)))
       ;(token (string ">=")*> return (fun l r -> Ast.Bin(Ast.Geq,l,r)))
       ;(token (string "<")*> return (fun l r -> Ast.Bin(Ast.Lt,l,r)))
       ;(token (string "=")*> return (fun l r -> Ast.Bin(Ast.Eq,l,r)))
       ;(token (string ">")*> return (fun l r -> Ast.Bin(Ast.Gt,l,r)))]
    in chainl1(pack.add_sub pack) op
  in
  { single; apps; unary; mul_div; add_sub; comp }
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.comp parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (Parsing_error er)
;;