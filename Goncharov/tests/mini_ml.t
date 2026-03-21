Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: LGPL-3.0-or-later

Cram tests for miniML interpreter

  $ ./mini_cli.exe <<'EOF'
  > 1 + 2 * 3
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > (1 + 2) * 3
  > EOF
  Success: 9

  $ ./mini_cli.exe <<'EOF'
  > 10 - 3 - 2
  > EOF
  Success: 5

  $ ./mini_cli.exe <<'EOF'
  > 8 / 2 + 1
  > EOF
  Success: 5

=== Basic lambdas ===

  $ ./mini_cli.exe <<'EOF'
  > fun x -> x
  > EOF
  Success: fun x -> x

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x) 42
  > EOF
  Success: 42

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> fun y -> x) 1 2
  > EOF
  Success: 1

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> fun x -> x) 1 2
  > EOF
  Success: 2

=== Multi-argument functions (desugaring) ===

  $ ./mini_cli.exe <<'EOF'
  > let add x y = x + y in add 3 4
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > (fun x y z -> x + y + z) 1 2 3
  > EOF
  Success: 6

=== CBV evaluation order ===

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x + x) (1 + 2)
  > EOF
  Success: 6

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> 5) (1 / 0)
  > EOF
  Interpreter error: Division by zero

=== Arithmetic with negatives ===

  $ ./mini_cli.exe <<'EOF'
  > -3 + 10 / 2
  > EOF
  Success: 2

  $ ./mini_cli.exe <<'EOF'
  > -(5 - 3)
  > EOF
  Success: -2

  $ ./mini_cli.exe <<'EOF'
  > -(-4)
  > EOF
  Success: 4

=== Comparison operators ===

  $ ./mini_cli.exe <<'EOF'
  > if 2 <= 3 then 7 else 9
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > if 3 >= 4 then 1 else 0
  > EOF
  Success: 0

  $ ./mini_cli.exe <<'EOF'
  > if 5 = 5 then 1 else 0
  > EOF
  Success: 1

  $ ./mini_cli.exe <<'EOF'
  > if 5 <> 5 then 1 else 0
  > EOF
  Success: 0

  $ ./mini_cli.exe <<'EOF'
  > if 3 < 5 then 1 else 0
  > EOF
  Success: 1

  $ ./mini_cli.exe <<'EOF'
  > if 5 > 3 then 1 else 0
  > EOF
  Success: 1

=== Nested conditions ===

  $ ./mini_cli.exe <<'EOF'
  > if 1 then if 0 then 2 else 3 else 4
  > EOF
  Success: 3

  $ ./mini_cli.exe <<'EOF'
  > if (if 1 then 0 else 1) then 5 else 6
  > EOF
  Success: 6

=== If with functions ===

  $ ./mini_cli.exe <<'EOF'
  > (if 1 then fun x -> x + 1 else fun x -> x + 2) 4
  > EOF
  Success: 5

  $ ./mini_cli.exe <<'EOF'
  > (if 0 then fun x -> x * 2 else fun x -> x * 3) 5
  > EOF
  Success: 15

=== Let expressions ===

  $ ./mini_cli.exe <<'EOF'
  > let x = 5 in let y = 2 in x - y
  > EOF
  Success: 3

  $ ./mini_cli.exe <<'EOF'
  > let f = fun x -> x + 1 in f 10
  > EOF
  Success: 11

  $ ./mini_cli.exe <<'EOF'
  > let x = 1 in let x = 2 in x
  > EOF
  Success: 2

  $ ./mini_cli.exe <<'EOF'
  > let x = 1 in (let x = 2 in x) + x
  > EOF
  Success: 3

=== Recursive functions (let rec) ===

  $ ./mini_cli.exe <<'EOF'
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 4
  > EOF
  Success: 24

  $ ./mini_cli.exe <<'EOF'
  > let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5
  > EOF
  Success: 120

  $ ./mini_cli.exe <<'EOF'
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10
  > EOF
  Success: 55

  $ ./mini_cli.exe <<'EOF'
  > let rec sum n = if n <= 0 then 0 else n + sum (n - 1) in sum 100
  > EOF
  Success: 5050

=== Fix combinator ===

  $ ./mini_cli.exe <<'EOF'
  > fix (fun fib n -> if n <= 1 then n else fib (n - 1) + fib (n - 2)) 6
  > EOF
  Success: 8

  $ ./mini_cli.exe <<'EOF'
  > let fact = fix (fun self n -> if n <= 1 then 1 else n * self (n - 1)) in fact 5
  > EOF
  Success: 120

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f x -> if x <= 0 then 1 else x * f (x - 1)) 4
  > EOF
  Success: 24

=== Print builtin ===

  $ ./mini_cli.exe <<'EOF'
  > let a = 10 in let _ = print (a * a) in a
  > EOF
  100
  Success: 10

  $ ./mini_cli.exe <<'EOF'
  > print 42
  > EOF
  42
  Success: 0

  $ ./mini_cli.exe <<'EOF'
  > if 1 then print 1 else print 0
  > EOF
  1
  Success: 0

=== Overriding builtins (requirement: users can shadow print/fix) ===

  $ ./mini_cli.exe <<'EOF'
  > let print = 42 in print + 1
  > EOF
  Success: 43

  $ ./mini_cli.exe <<'EOF'
  > let fix = 10 in fix * 2
  > EOF
  Success: 20

  $ ./mini_cli.exe <<'EOF'
  > let print = fun x -> x + 1 in print 5
  > EOF
  Success: 6

=== Application and operator precedence ===

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x) 4 + 5
  > EOF
  Success: 9

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x + 1) 2 * 3
  > EOF
  Success: 9

  $ ./mini_cli.exe <<'EOF'
  > fun x -> x + 1 * 2
  > EOF
  Success: fun x -> (x + (1 * 2))

=== Error cases ===

  $ ./mini_cli.exe <<'EOF'
  > x
  > EOF
  Interpreter error: Unbound variable

  $ ./mini_cli.exe <<'EOF'
  > 1 / 0
  > EOF
  Interpreter error: Division by zero

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x) + 1
  > EOF
  Interpreter error: Type error: no such binary operation or operands are not integers

  $ ./mini_cli.exe <<'EOF'
  > - (fun x -> x)
  > EOF
  Interpreter error: Type error: not integer in Ast.Neg

  $ ./mini_cli.exe <<'EOF'
  > if (fun x -> x) then 1 else 2
  > EOF
  Interpreter error: Type error: condition is not integer

  $ ./mini_cli.exe <<'EOF'
  > 1 2
  > EOF
  Interpreter error: Type error: error during app

  $ ./mini_cli.exe <<'EOF'
  > let rec f = 5 in f 1
  > EOF
  Interpreter error: Type error: expected function on the right

=== Parsing errors (keywords cannot be variable names) ===

  $ ./mini_cli.exe <<'EOF'
  > let fun = 1 in fun
  > EOF
  Parsing error
  [1]

  $ ./mini_cli.exe <<'EOF'
  > let if = 1 in if
  > EOF
  Parsing error
  [1]

  $ ./mini_cli.exe <<'EOF'
  > let then = 1 in then
  > EOF
  Parsing error
  [1]

=== Infinite loop detection ===

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f -> f) 0
  > EOF
  Interpreter error: Unknown variable: steps limit exceeded

  $ ./mini_cli.exe <<'EOF'
  > let rec loop = fun x -> loop x in loop 1
  > EOF
  Interpreter error: Unknown variable: steps limit exceeded

=== Complex expressions ===

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> (fun y -> x + y) 3) 4
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > let f = fun x -> if x <= 0 then 1 else x * f (x - 1) in f 5
  > EOF
  Interpreter error: Unbound variable

  $ ./mini_cli.exe <<'EOF'
  > let rec f = fun x -> if x <= 0 then 1 else x * f (x - 1) in f 5
  > EOF
  Success: 120

=== Parentheses and associativity ===

  $ ./mini_cli.exe <<'EOF'
  > 1 + (2 + 3)
  > EOF
  Success: 6

  $ ./mini_cli.exe <<'EOF'
  > (1 + 2) + 3
  > EOF
  Success: 6

  $ ./mini_cli.exe <<'EOF'
  > 2 * 3 + 4 * 5
  > EOF
  Success: 26

=== Functions as values ===

  $ ./mini_cli.exe <<'EOF'
  > let id = fun x -> x in id id 5
  > EOF
  Success: 5

  $ ./mini_cli.exe <<'EOF'
  > let compose f g x = f (g x) in compose (fun x -> x + 1) (fun x -> x * 2) 3
  > EOF
  Success: 7
