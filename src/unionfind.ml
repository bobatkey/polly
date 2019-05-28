type 'a class_data =
  | Root of int * 'a
  | Node of 'a equiv_class

and 'a equiv_class =
  'a class_data ref

let make_class x =
  ref (Root (0, x))

let rec get_root point = match !point with
  | Root (rank, a) ->
    (point, rank, a)
  | Node parent    ->
    let root, rank, a = get_root parent in
    point := Node root;
    (root, rank, a)

let find point =
  let _, _, a = get_root point in a

let union x y =
  let x_root, x_rank, _      = get_root x in
  let y_root, y_rank, y_info = get_root y in
  if x_rank > y_rank then
    (y_root := Node x_root;
     x_root := Root (x_rank, y_info))
  else if x_rank < y_rank then
    x_root := Node y_root
  else if x_root != y_root then
    (y_root := Node x_root;
     x_root := Root (x_rank + 1, y_info))
  else
    ()

let equiv x y =
  let x_root, _, _ = get_root x in
  let y_root, _, _ = get_root y in
  x_root == y_root

let set point a =
  let root, rank, _ = get_root point in
  root := Root (rank, a)
