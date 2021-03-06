let errorf fmt =
  Format.kasprintf (fun e -> Error e) fmt

module Infix = struct
  let (>>=) x f = match x with
    | Ok x         -> f x
    | Error _ as e -> e
end

open Infix

let rec iter f = function
  | []    -> Ok ()
  | x::xs -> f x >>= fun () -> iter f xs

let rec traverse f = function
  | [] ->
    Ok []
  | x::xs ->
    f x           >>= fun y ->
    traverse f xs >>= fun ys ->
    Ok (y::ys)

let map_err f = function
  | Ok _ as x -> x
  | Error e   -> Error (f e)

let pp ~ok ~error fmt = function
  | Ok a    -> Format.fprintf fmt "Ok (%a)" ok a
  | Error e -> Format.fprintf fmt "Error (%a)" error e
