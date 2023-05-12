open Expression

exception SyntaxError of int * string

type pos = int

type token =
  | LPAREN of pos
  | RPAREN of pos
  | PLUS of pos
  | MINUS of pos
  | TIMES of pos
  | DIVIDE of pos
  | POWER of pos
  | VARIABLE of pos * string
  | FUNCTION of pos * string
  | NUMBER of pos * int

let print_token = function
  | LPAREN pos -> Printf.printf "('(', %d)" pos
  | RPAREN pos -> Printf.printf "(')', %d)" pos
  | PLUS pos -> Printf.printf "('+', %d)" pos
  | MINUS pos -> Printf.printf "('-', %d)" pos
  | TIMES pos -> Printf.printf "('*', %d)" pos
  | DIVIDE pos -> Printf.printf "('/', %d)" pos
  | POWER pos -> Printf.printf "('^', %d)" pos
  | NUMBER (pos, n) -> Printf.printf "('%d', %d)" n pos
  | VARIABLE (pos, v) -> Printf.printf "('%s', %d)" v pos
  | FUNCTION (pos, f) -> Printf.printf "('%s', %d)" f pos

let print_tokens tokens =
  let rec aux = function
    | [] -> ()
    | t :: ts ->
        let () = print_token t in
        aux ts
  in
  aux tokens

let rec parseString line j acc min max =
  if j >= String.length line
  then (acc, j)
  else
    match String.get line j with
    | c when c >= min && c <= max ->
        parseString line (j + 1) (String.make 1 c :: acc) min max
    | _ -> (acc, j)

let parseNumber line j acc = parseString line j acc '0' '9'

let parseLiteral line j acc =
  let lstring, i = parseString line j acc 'a' 'z' in
  let string = String.concat "" (List.rev lstring) in
  if String.length string = 1
  then (VARIABLE (j, string), i)
  else (FUNCTION (j, string), i)

let displayError error i line =
  if String.length line = 0
  then
    let spaces = if i = -1 then String.length line else i in
    Printf.fprintf stderr "\n%s\n%s^\nError : %s (at char %d)\n" line
      (String.make spaces ' ') error i
  else Printf.fprintf stderr "Error : %s\n" error

let tokenise line =
  let rec aux i acc =
    if i >= String.length line
    then acc
    else
      match String.get line i with
      | '(' -> aux (i + 1) (LPAREN i :: acc)
      | ')' -> aux (i + 1) (RPAREN i :: acc)
      | '+' -> aux (i + 1) (PLUS i :: acc)
      | '-' -> aux (i + 1) (MINUS i :: acc)
      | '*' -> aux (i + 1) (TIMES i :: acc)
      | '/' -> aux (i + 1) (DIVIDE i :: acc)
      | '^' -> aux (i + 1) (POWER i :: acc)
      | ' ' -> aux (i + 1) acc
      | '0' .. '9' ->
          let digits, j = parseNumber line i [] in
          let number = int_of_string (String.concat "" (List.rev digits)) in
          aux j (NUMBER (i, number) :: acc)
      | 'a' .. 'z' ->
          let token, j = parseLiteral line i [] in
          aux j (token :: acc)
      | _ as c ->
          raise (SyntaxError (i, "Unexpected character : " ^ String.make 1 c))
  in
  try List.rev (aux 0 [])
  with SyntaxError (i, error) ->
    let () = displayError error i line in
    []

let rec factor = function
  | [] -> raise (SyntaxError (-1, "Expected an expression but got nothing"))
  | NUMBER (_, n) :: t -> (
      match t with
      | VARIABLE _ :: _ | FUNCTION _ :: _ ->
          let factor, rest = factor t in
          (mul (number n) factor, rest)
      | LPAREN _ :: _ ->
          let expression, rest = expr None 0 t in
          (mul (number n) expression, rest)
      | _ -> (number n, t))
  | VARIABLE (_, v) :: t -> (var v, t)
  | MINUS p :: t -> (
      try
        let exp, rest = factor t in
        (neg exp, rest)
      with SyntaxError (ep, error) ->
        if ep = -1
        then raise (SyntaxError (p, "Expected an expression after '-'"))
        else raise (SyntaxError (ep, error)))
  | PLUS p :: t -> (
      try
        let exp, rest = factor t in
        (exp, rest)
      with SyntaxError (ep, error) ->
        if ep = -1
        then raise (SyntaxError (p, "Expected an expression after '+'"))
        else raise (SyntaxError (ep, error)))
  | LPAREN p :: t -> (
      let exp, rest = expr None 0 t in
      match rest with
      | RPAREN _ :: t -> (exp, t)
      | _ -> raise (SyntaxError (p, "Expected a ')'")))
  | DIVIDE p :: _ | TIMES p :: _ | POWER p :: _ | RPAREN p :: _ ->
      raise (SyntaxError (p, "Unexpected token"))
  | FUNCTION (p, f) :: t -> (
      match t with
      | LPAREN p :: t -> (
          let exp, rest = expr None 0 t in
          match rest with
          | RPAREN _ :: t -> (func f exp, t)
          | _ -> raise (SyntaxError (p, "Expected a ')'")))
      | _ -> raise (SyntaxError (p + String.length f, "Expected a '('")))

and expr acc level = function
  | [] -> raise (SyntaxError (-1, "Expected an expression but got nothing"))
  | t -> (
      if level >= 3
      then factor t
      else
        let leftExpr, rest =
          if acc = None then expr acc (level + 1) t else (Option.get acc, t)
        in
        match rest with
        | [] -> (leftExpr, [])
        | PLUS _ :: t when level = 0 ->
            let rightExpr, rightRest = expr None 0 t in
            (add leftExpr rightExpr, rightRest)
        | MINUS _ :: t when level = 0 ->
            let rightExpr, rightRest = expr None 0 t in
            (sub leftExpr rightExpr, rightRest)
        | TIMES _ :: t when level = 1 ->
            let rightExpr, rightRest = expr None 1 t in
            (mul leftExpr rightExpr, rightRest)
        | DIVIDE _ :: t when level = 1 ->
            let rightExpr, rightRest = expr None 1 t in
            (div leftExpr rightExpr, rightRest)
        | POWER _ :: t when level = 2 ->
            let rightExpr, rightRest = expr None 2 t in
            (pow leftExpr rightExpr, rightRest)
        | LPAREN p :: _ ->
            raise
              (SyntaxError (p, "Unexpected parenthesis, expected an operator"))
        | _ -> (leftExpr, rest))

let is_list_empty = function
  | [] -> true
  | _ -> false

let parse line =
  let tokens = tokenise line in
  if is_list_empty tokens
  then (
    flush stderr;
    empty ())
  else
    try
      let exp, rest = expr None 0 tokens in
      match rest with
      | [] -> exp
      | h :: _ -> (
          match h with
          | LPAREN p -> raise (SyntaxError (p, "Expected a ')'"))
          | RPAREN p -> raise (SyntaxError (p, "Unexpected ')'"))
          | PLUS p | MINUS p | TIMES p | DIVIDE p | POWER p ->
              raise (SyntaxError (p, "Unexpected operator"))
          | NUMBER (p, _) -> raise (SyntaxError (p, "Unexpected number"))
          | VARIABLE (p, _) -> raise (SyntaxError (p, "Unexpected variable"))
          | FUNCTION (p, _) ->
              raise (SyntaxError (p, "Unexpected function name")))
    with SyntaxError (i, error) ->
      displayError error i line;
      flush stderr;
      empty ()
