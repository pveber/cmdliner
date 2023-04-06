open Cmdliner

let echo _ _ _ msg =
  print_endline msg

let foo = Arg.(opt string "" & info ["foo"])
let msg = Arg.(pos 0 (some string) None (info []))

let echo_term =
  Term.(
    const echo
    $ Arg.(value & flag & info ~doc:"Answer no to all" ["n";"no"])
    $ Arg.(value & flag & info ["no-tty"])
    $ Arg.(value foo)
    $ Arg.(required msg)
  )

let echo_info = Cmd.info "echo"

let echo_cmd = Cmd.v echo_info echo_term

let nop_cmd =
  let term = Term.(const ())
  in
  let info = Cmd.info "nop" in
  Cmd.v info term

let cmd =
  Cmd.group
    (Cmd.info "test_completion")
    [ echo_cmd ; nop_cmd ]

let repeat_completion =
  Term.const (
    fun tok -> [`Value (tok ^ tok)]
  )

let completions = [
  Completion.handler foo repeat_completion ;
  Completion.handler msg repeat_completion ;
]

let string_of_proposal p =
  let v, descr, complete = match p with
  | `Subcmd (name, descr)
  | `Option (name, descr) -> name, descr, true
  | `Value v -> v, "", true
  in
  Printf.sprintf "%20s | %20s | %b" v descr complete

let completion_test fragment =
  let completions = Completion.complete completions cmd fragment in
  Printf.printf "Fragment: %s\n" fragment ;
  List.iter
    (fun p -> print_endline (string_of_proposal p))
    completions ;
  print_newline ()

let () =
  completion_test "" ;
  completion_test "e" ;
  completion_test "echo" ;
  completion_test "echo " ;
  completion_test "echo -" ;
  completion_test "echo --" ;
  completion_test "echo --no-" ;
  completion_test "echo --foo ab" ;
  completion_test "echo --foo ab a" ;
  ()
