let print_hello tab n =
    print_endline "Hello World";
      Array.iter (fun n -> Printf.printf "%d " n) tab;
      print_endline "";
      Printf.printf "%d\n" n;
      print_endline "Hello World";
      "BDDBBGHHDHGBDDBBGGHDDBGGGHHDBBGHHHDBGHDDDBGGBBGHHH"

(*
let print_hello () =
    print_endline "Hello World"
*)

let () =
      Callback.register "Hello callback" print_hello
