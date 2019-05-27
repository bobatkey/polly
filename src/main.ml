let split_on_equal str =
  let idx = String.index str '=' in
  String.sub str 0 idx,
  String.sub str (idx+1) (String.length str - idx - 1)

let read_file filename =
  let ch = open_in filename in
  match
    Parser.program Lexer.token (Lexing.from_channel ch)
  with
    | result -> close_in ch; result
    | exception e -> close_in ch; raise e

let exec_and_print arguments program =
  Lwt.bind (Evaluator.eval arguments program)
    (function
      | Ok value ->
         Lwt_fmt.printf "Ok (%a)@." Evaluator.Value.pp value
      | Error msg ->
         Lwt_fmt.printf "Error %S@." msg)

let () =
  let filename, arguments =
    match Array.to_list Sys.argv with
      | _::filename::args ->
         filename, List.map split_on_equal args
      | _ ->
         failwith "bad args"
  in
  let ast = read_file filename in
  match Polly.Checker.check_program ast with
    | Error msg ->
       prerr_endline msg;
       exit 1
    | Ok program ->
       Lwt_main.run (exec_and_print arguments program)
