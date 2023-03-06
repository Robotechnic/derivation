type token

exception SyntaxError of int * string
(**
	Syntax error at line number [1] with message [2].
*)

val tokenise : string -> token list
(**
	Parse the string [1] and return the corresponding expression.
	@raise SyntaxError if the string is not a valid expression.
	@param [1] the string to parse.
	@return the expression corresponding to the string.
*)

val parse : string -> Expression.expr
(**
	Parse the list of tokens [1] and return the corresponding expression.
	@raise SyntaxError if the list of tokens is not a valid expression.
	@param [1] the line to parse.
	@return the expression corresponding to the list of tokens.
*)

val print_tokens : token list -> unit
(**
	Print the list of tokens [1] on the standard output.
	@param [1] the list of tokens to print.
*)