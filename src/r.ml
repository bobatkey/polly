let errorf fmt =
  Printf.ksprintf (fun e -> Error e) fmt

module Infix = struct
  let (>>=) x f = match x with
    | Ok x         -> f x
    | Error _ as e -> e
end

open Infix

let rec iter f = function
  | []    -> Ok ()
  | x::xs -> f x >>= fun () -> iter f xs

let map_err f = function
  | Ok _ as x -> x
  | Error e   -> Error (f e)

let pp ~ok ~error fmt = function
  | Ok a    -> Format.fprintf fmt "Ok (%a)" ok a
  | Error e -> Format.fprintf fmt "Error (%a)" error e
