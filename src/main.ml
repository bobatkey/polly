let split_on_equal str =
  let idx = String.index str '=' in
  String.sub str 0 idx,
  String.sub str (idx+1) (String.length str - idx - 1)

let read_file filename =
  let ch = open_in filename in
  match
    let lexbuf = Lexing.from_channel ch in
    (let open Lexing in
     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename });
    Parser.program Lexer.token lexbuf
  with
    | result -> close_in ch; result
    | exception e -> close_in ch; raise e

let exec_and_print arguments program =
  let open Lwt.Infix in
  Evaluator.eval arguments program
  >>= fun result ->
  Lwt_io.flush Lwt_io.stderr
  >>= fun () -> match result with
  | Ok (Evaluator.Value.Json json) ->
    let str = Yojson.Basic.to_string json in
    Lwt_fmt.printf "%s@." str
    >|= fun () ->
    0
  | Ok v ->
    Lwt_fmt.eprintf
      "Error: unexpected value returned: (@[%a@])@."
      Evaluator.Value.pp v
    >|= fun () ->
    1
  | Error msg ->
    Lwt_fmt.eprintf "Error %S@." msg
    >|= fun () ->
    1

let () =
  let filename, arguments =
    match Array.to_list Sys.argv with
      | _::filename::args ->
         filename, List.map split_on_equal args
      | _ ->
         failwith "bad args"
  in
  let ast = read_file filename in
  match Polly.Checker.check_program ast (Checker.AbstrSort Polly.Json) with
    | Error msg ->
      prerr_endline msg;
      exit 1
    | Ok program ->
      let rv = Lwt_main.run (exec_and_print arguments program) in
      exit rv
