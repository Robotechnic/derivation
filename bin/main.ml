open Parser
open Expression

let usageMsg = "Usage: " ^ Sys.argv.(0) ^ " [-v VARIABLE] [-e EXPRESSION] [-h]"
let variable = ref "x"
let expression = ref ""

let specList =
  [
    ("-v", Arg.Set_string variable, "Variable to derivate");
    ("-e", Arg.Set_string expression, "Expression to derivate");
  ]

let stdinDerivate () =
  while true do
    try
      let line = read_line () in
      let expression = parse line in
      print_expression (derivate expression !variable);
      print_newline ()
    with End_of_file -> ()
  done

let checkVariable () =
  if !variable = ""
  then (
    Printf.eprintf "No variable specified\n";
    exit 1)
  else if String.length !variable > 1
  then (
    Printf.eprintf "Variable must be a single character\n";
    exit 1)
  else if !variable.[0] < 'a' || !variable.[0] > 'z'
  then (
    Printf.eprintf "Variable must be a lowercase letter\n";
    exit 1)

let () =
  Arg.parse specList
    (fun arg -> failwith ("Unexpected argument: " ^ arg))
    usageMsg;
  checkVariable ();
  if !expression = ""
  then stdinDerivate ()
  else
    let expression = parse !expression in
    print_expression (derivate expression !variable);
    print_newline ()
