type expr

val print_expression : expr -> unit
(**
	display an expression with correct format
	Exemple:
		- print_expression (add (var "x") (number 2)) -> x + 2
		- print_expression add (mul (var "x") (number 2)) (var "x")-> x * 2 + x
	
	@params [1] : expression to display
*)

val var : string -> expr
(** Instanciate a new variable with the given name

    @param [1] string : name of the variable
    @return expr : the new variable *)

val number : int -> expr
(** Instanciate a new number

    @param [1] int : value of the number
    @return expr : the new number *)

val func : string -> expr -> expr
(** Instanciate a new function with the given name

    @param [1] string : name of the function
    @param [2] expr : expression to apply to the function
    @return expr : the new function *)

val add : expr -> expr -> expr
(** Instanciate a new expression representing the sum of two expressions It's
    abble detects some spetial cases like 0 + x = x or x + x = 2 * x

    @param [1] expr : first expression to add
    @param [2] expr : second expression to add
    @return expr : first expression added by the second expression *)

val sub : expr -> expr -> expr
(** Instanciate a new expression representing the substraction of two
    expressions It's abble detects some spetial cases like 0 - x = -x or x - x =
    0

    @param [1] expr : first expression to substract
    @param [2] expr : second expression to substract
    @return expr : first expression substracted by the second expression *)

val mul : expr -> expr -> expr
(** Instanciate a new expression representing the multiplication of two
    expressions It's abble detects some spetial cases like 0 * x = 0 or x * x =
    x^2

    @param [1] expr : first expression to multiply
    @param [2] expr : second expression to multiply
    @return expr : first expression multiplied by the second expression *)

val div : expr -> expr -> expr
(** Instanciate a new expression representing the division of two expressions
    It's abble detects some spetial cases like 0 / x = 0 or x / x = 1

    @param [1] expr : first expression to divide
    @param [2] expr : second expression to divide
    @return expr : first expression divided by the second expression *)

val neg : expr -> expr
(** Instanciate a new expression representing the negation of the given
    expression

    @param [1] expr : expression to negate
    @return expr : negation of the given expression *)

val pow : expr -> expr -> expr
(** Instanciate a new expression representing the power of two expressions It's
    abble detects some spetial cases like x^0 = 1 or x^1 = x

    @param [1] expr : first expression to power
    @param [2] expr : second expression to power it need to be a number
    @return expr : first expression powered by the second expression *)

val empty : unit -> expr
(** Instanciate a new empty expression

    @return expr : the new empty expression *)

val is_empty : expr -> bool
(** Check if the given expression is empty

    @param [1] expr : expression to check
    @return bool : true if the expression is empty, false otherwise *)

val derivate : expr -> string -> expr
(** Compute the derivate of the given expression with respect to the given
    variable

    @param [1] expr : expression to derivate
    @param [2] string : variable with respect to derivate
    @return expr : derivate of the given expression *)
