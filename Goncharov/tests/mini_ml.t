  $ ./mini_cli.exe << 'EOF'
  > 1 + 2 * 3
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > let rec fact = fun n -> if n = 1 then 1 else n * fact (n-1) in fact 4
  > EOF
  Success: 24

  $ ./mini_cli.exe <<'EOF'
  > fix (fun fib -> fun n -> if n = 0 then 0 else if n = 1 then 1 else fib (n-1) + fib (n-2)) 6
  > EOF
  Success: 8

  $ ./mini_cli.exe <<'EOF'
  > -3 + 10 / 2
  > EOF
  Success: 2

  $ ./mini_cli.exe <<'EOF'
  > let add = fun x -> fun y -> x + y in add 2 3
  > EOF
  Success: 5

  $ ./mini_cli.exe <<'EOF'
  > let x = 5 in let y = 2 in x - y
  > EOF
  Success: 3

  $ ./mini_cli.exe <<'EOF'
  > if 2 <= 3 then 7 else 9
  > EOF
  Success: 7

  $ ./mini_cli.exe <<'EOF'
  > if 3 >= 4 then 1 else 0
  > EOF
  Success: 0

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f -> fun x -> x + 1) 4
  > EOF
  Success: 5

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f -> fun n -> f n) 0
  > EOF
  Interpreter error: Unknown variable: steps limit exceeded

  $ ./mini_cli.exe <<'EOF'
  > x
  > EOF
  Interpreter error: Unbound variable

  $ ./mini_cli.exe <<'EOF'
  > x -> x -> x + x
  > EOF
  Interpreter error: Unknown variable: cannot represent function as integer

  $ ./mini_cli.exe <<'EOF'
  > 1 / 0
  > EOF
  Interpreter error: Division by zero

  $ ./mini_cli.exe <<'EOF'
  > - (fun x -> x)
  > EOF
  Interpreter error: Type error: not integer in Ast.Neg

  $ ./mini_cli.exe <<'EOF'
  > (fun x -> x) + 1
  > EOF
  Interpreter error: Type error: no such binary operation or operands are not integers

  $ ./mini_cli.exe <<'EOF'
  > 1 2
  > EOF
  Interpreter error: Type error: error during app

  $ ./mini_cli.exe <<'EOF'
  > if (fun x -> x) then 1 else 2
  > EOF
  Interpreter error: Type error: condition is not integer

  $ ./mini_cli.exe <<'EOF'
  > let rec f = 1 in f
  > EOF
  Interpreter error: Type error: expected function on the right

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f -> 1)
  > EOF
  Interpreter error: Type error: fix expects function returning function

  $ ./mini_cli.exe <<'EOF'
  > fix 1
  > EOF
  Interpreter error: Type error: expected function

  $ ./mini_cli.exe << 'EOF'
  > let fun = 1 in fun
  > EOF
  Parsing error

  $ ./mini_cli.exe << 'EOF'
  > let ifx a b = a+b in
  > let theny = 2 in 
  > let elsez = 1 in 
  > ifx theny elsez
  > EOF
  Success: 3

  $ ./mini_cli.exe << 'EOF'
  > let x a b = a+b in
  > let theny = 0 in
  > let elsez = 1 in
  > ifx theny elsez then 1 else 2
  > EOF
  Parsing error

  $ ./mini_cli.exe << 'EOF'
  > let x = 1 inx
  > EOF
  Parsing error
