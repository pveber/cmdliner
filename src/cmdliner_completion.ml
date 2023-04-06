  type 'a proposal = [
    | `Subcmd of string * string
    | `Option of string * string
    | `Value of 'a
  ]

type 'a handler = Cmdliner_info.Arg.t * (string -> 'a proposal list) Cmdliner_term.t

(* FIXME: explain invariant that an arg is a term with a single arg
   info in its argset *)
let info_of_arg (argset, _) =
  match Cmdliner_info.Arg.Set.elements argset with
  | [ i ] -> i
  | _ -> assert false

let handler arg term = info_of_arg arg, term

let identify_command cmd raw_args =
  let find_candidates cmds k =
    List.find_all
      (fun child -> String.starts_with ~prefix:k (Cmdliner_cmd.name child))
      cmds
  in
  let cmd_completion cmd =
    let i = Cmdliner_cmd.get_info cmd in
    `Subcmd Cmdliner_info.Cmd.(name i, doc i)
  in
  let rec traverse args parents = function
  | Cmdliner_cmd.Cmd (i, _) -> `Identified_command (i, parents, args)
  | Group (i, (_, children)) -> (
      let arg_to_be_completed, remaining_args =
        match args with
        | [] -> "", []
        | h :: t -> h, t
      in
      match find_candidates children arg_to_be_completed with
      | [ subcmd ] -> (
          if Cmdliner_cmd.name subcmd = arg_to_be_completed then
            traverse remaining_args (i :: parents) subcmd
          else `Unidentified_command [ cmd_completion subcmd ]
        )
      | candidates ->
          `Unidentified_command (
            List.map cmd_completion candidates
          )
    )
  in
  traverse raw_args [] cmd

let complete_option_name opt_set tok =
  let module Arg = Cmdliner_info.Arg in
  let f =
    match tok with
    | `Empty | `Single_dash -> Fun.const true
    | `Double_dash -> (fun n -> String.length n > 2)
    | `Short opt_name -> (String.equal opt_name)
    | `Long prefix -> (String.starts_with ~prefix)
  in
  Arg.Set.elements opt_set
  |> List.concat_map (fun a ->
      let descr = Arg.doc a in
      List.filter_map
        (fun n -> if f n then Some (`Option (n, descr)) else None)
        (Arg.opt_names a)
    )

let complete_value ~env completions cmd parents arg cline partial_value =
  let module Arg = Cmdliner_info.Arg in
  match Arg.Map.find_opt arg completions with
  | None -> []
  | Some (handler_term : _ Cmdliner_term.t) ->
      let eval_env = Cmdliner_info.Eval.v ~cmd ~env ~parents ~err_ppf:Format.err_formatter in
      match (snd handler_term) eval_env cline with
      | Ok f -> f partial_value
      | _ -> []

let basic_cmd_completion env cmd parents raw_args completions =
  let arg_set = Cmdliner_info.Cmd.args cmd in
  match Cmdliner_cline.partial_parsing arg_set raw_args with
  | None -> []
  | Some (incomplete_token, cl) -> (
      match incomplete_token with
      | Incomplete_option_name tok ->
          complete_option_name arg_set tok
      | Incomplete_value iov -> (
          match iov.maybe_arg with
          | None -> []
          | Some arg ->
              complete_value ~env completions cmd parents arg cl iov.partial_value
        )
      | Empty { maybe_pos_arg } ->
          (
            match maybe_pos_arg with
            | None -> []
            | Some a -> complete_value ~env completions cmd parents a cl ""
          ) @ complete_option_name arg_set `Empty
    )

let env_default v = try Some (Sys.getenv v) with Not_found -> None

let complete
    ?(env = env_default)
    (completions : 'a handler list) cmd fragment : 'a proposal list =
  let completions =
    List.fold_left
      (fun acc (arg, term) -> Cmdliner_info.Arg.Map.add arg term acc)
      Cmdliner_info.Arg.Map.empty
      completions
  in
  match Cmdliner_base.tokenize fragment with
  | None -> []
  | Some argv ->
      match identify_command cmd argv with
      | `Unidentified_command completions -> completions
      | `Identified_command (cmd_infos, parents, args) ->
          basic_cmd_completion env cmd_infos parents args completions
