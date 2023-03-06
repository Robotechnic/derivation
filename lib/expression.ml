type expr =
  | Var    of int*string*int
  | Number of int
  | Add    of expr * expr
  | Sub    of expr * expr
  | Mul    of expr * expr
  | Div    of expr * expr
  | Pow    of expr * expr
  | Func   of string * expr

let var name = Var(1,name,1)
let cpvar coeff name pow = if coeff = 0 then Number(0) else if pow = 0 then Number(coeff) else Var(coeff, name, pow)
let number n = Number n

let func name expr = Func(name, expr)

let priority = function
  | Var _ | Number _ -> 0
  | Add _ | Sub _ -> 1
  | Mul _ | Div _ -> 2
  | Pow _ -> 3
  | Func _ -> 4

let charOfOp = function
  | Add _ -> '+'
  | Sub _ -> '-'
  | Mul _ -> '*'
  | Div _ -> '/'
  | Pow _ -> '^'
  | _ -> failwith "Not an operator"


let parentheses parent child str =
  let childPriority = priority child in
  if childPriority <> 0 && priority parent > childPriority then
    Printf.sprintf "(%s)" str
  else
    str
(**
    Display parenthesis if the child has a lower priority than the parent
    e.g. 2 * (3 + 4) instead of 2 * 3 + 4
    @param [1] parent expression
    @param [2] child expression
    @param [3] string representation of the child expression
    @return string representation of the child expression with parenthesis if needed
*)

let print_expression e =
  let rec print e =
    match e with
    | Var (coeff, name, power) -> 
      (if coeff = 1 then "" else if coeff = -1 then "-" else (string_of_int coeff)) ^ 
      name ^
      (if power = 1 then "" else "^" ^ (string_of_int power))
    | Number n -> string_of_int n
    | Func (name, body) -> name ^ "(" ^ (print body) ^ ")"
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) | Pow (e1, e2) ->
      Printf.sprintf "%s %c %s" (parentheses e e1 (print e1)) (charOfOp e) (parentheses e e2 (print e2))
  in
  print_string (print e)


let add e1 e2 =
  match e1, e2 with
  | Number 0, _ -> e2
  | _, Number 0 -> e1
  | Number n1, Number n2 -> Number (n1 + n2)
  | Var (coeff1,s1,pow1), Var (coeff2, s2, pow2) when s1 = s2 && pow1 = pow2 -> cpvar (coeff1 + coeff2) s1 pow1
  | _, _ -> if e1 = e2 then Mul(Number 2, e1) else Add(e1, e2)

let rec div e1 e2 =
  match e1, e2 with
  | Number 0, _ -> Number 0
  | _, Number 0 -> failwith "Division by zero"
  | _, Number 1 -> e1
  | Number n1, Number n2 when n1 mod n2 = 0-> Number (n1 / n2)
  | Var (coeff1,s1,pow1), Var (coeff2, s2, pow2) when s1 = s2 -> 
    if coeff1 mod coeff2 = 0 then 
      cpvar (coeff1 / coeff2) s1 (pow1 - pow2) 
    else 
      mul (Div (Number coeff1, Number coeff2)) (cpvar 1 s1 (pow1 - pow2))
  | Var (coeff1,s1,pow1), Number n2 when coeff1 mod n2 = 0 -> cpvar (coeff1 / n2) s1 pow1
  | Number n1, Number n2 when n1 mod n2 = 0 -> Number (n1 / n2)
  | _, _ -> if e1 = e2 then Number 1 else Div(e1, e2)
and mul e1 e2 =
  match e1, e2 with
  | Number 0, _ | _, Number 0 -> Number 0
  | Number 1, _ -> e2
  | _, Number 1 -> e1
  | Number n1, Number n2 -> Number (n1 * n2)
  | Number n1, Var (coeff2, s2, pow2) | Var (coeff2, s2, pow2), Number n1 -> cpvar (coeff2 * n1) s2 pow2
  | Number n1, Div (Number n2, e2) | Div (Number n2, e2), Number n1 -> div (Number (n1 * n2)) e2
  | Div (Number 1, e1), e2 when e1 = e2  -> Number 1
  | e1, Div (Number 1, e2) when e1 = e2  -> Number 1
  | Var (coeff1,s1,pow1), Var (coeff2, s2, pow2) when s1 = s2 -> cpvar (coeff1 * coeff2) s1 (pow1 + pow2)
  | _, _ -> if e1 = e2 then Pow(e1, Number 2) else Mul(e1, e2)

let sub e1 e2 =
  match e1, e2 with
  | Number 0, _ -> mul (Number (-1)) e2
  | _, Number 0 -> e1
  | Number n1, Number n2 -> Number (n1 - n2)
  | Var (coeff1,s1,pow1), Var (coeff2, s2, pow2) when s1 = s2 && pow1 = pow2 -> cpvar (coeff1 - coeff2) s1 pow1
  | _, _ -> if e1 = e2 then Number 0 else Sub(e1, e2)

let pow e1 e2 =
  match e1, e2 with
  | Number 0, _ -> Number 0
  | _, Number 0 -> Number 1
  | Number 1, _ -> Number 1
  | _, Number 1 -> e1
  | Number n1, Number n2 -> Number (int_of_float (float_of_int n1 ** float_of_int n2))
  | _, _ -> Pow(e1, e2)

let neg e = mul (Number (-1)) e

let derivateFunc name expr =
  match name with
  | "sin" -> Func ("cos", expr)
  | "cos" -> neg (Func ("sin", expr))
  | "tan" -> div (Number 1) (pow (Func ("cos", expr)) (Number 2))
  | "asin" -> div (Number 1) (Func ("sqrt", Sub (Number 1, Pow (var "x", Number 2))))
  | "acos" -> neg (div (Number 1) (Func ("sqrt", Sub (Number 1, Pow (var "x", Number 2)))))
  | "atan" -> div (Number 1) (Add(Number 1, Pow(var "x", Number 2)))
  | "exp" -> Func ("exp", expr)
  | "ln" -> div (Number 1) expr
  | "sqrt" -> div (Number 1) (mul (Number 2) (Func ("sqrt", expr)))
  | _ -> failwith "Function not supported"


let rec derivate expression variable = match expression with
  | Var (coeff, name, power) -> if name = variable then cpvar (coeff * power) name (power - 1) else Number 0
  | Number _ -> Number 0
  | Add (e1, e2) -> add (derivate e1 variable) (derivate e2 variable)
  | Sub (e1, e2) -> sub (derivate e1 variable) (derivate e2 variable)
  | Mul (e1, e2) -> add (mul (derivate e1 variable) e2) (mul e1 (derivate e2 variable))
  | Div (e1, e2) -> div (sub (mul (derivate e1 variable) e2) (mul e1 (derivate e2 variable))) (pow e2 (Number 2))
  | Pow (e1, Number n) -> mul (mul (Number n) (derivate e1 variable)) (pow e1 (Number (n - 1)))
  | Func (name, body) -> mul (derivateFunc name body) (derivate body variable)
  | Pow (e1, e2) -> mul (add (mul (derivate e2 variable) (Func ("ln", e1))) (mul (div (derivate e1 variable) e1) e2)) (pow e1 e2)

