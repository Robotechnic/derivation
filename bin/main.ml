open Parser
open Expression

let () = 
  while true do
    print_string ">> ";
    flush stdout;
    let line = read_line () in
    let expression = parse line in
    print_expression expression;
    print_newline ();
    print_expression (derivate expression "x");
    print_newline ();
  done