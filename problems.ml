(* 1.01 Find the last element of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | (_ :: xs) -> last xs

(* 1.02 Find the last but one element of a list *)
let rec last_but_one = function
  | [] -> None
  | [x;y] -> Some(x)
  | (_ :: xs) -> last_but_one xs

(* 1.03 Find the K'th element of a list *)
let rec find_kth k = function
  | [] -> None
  | (h :: t) -> 
    if k = 1 then Some(h)
    else find_kth (k - 1) t

(* 1.04 Find the number of elements in a list *)
let length_tailrec lst = 
  let rec aux acc = function
    | [] -> acc
    | (_ :: xs) -> aux (acc + 1) xs
    in aux 0 lst
    
let length = function
  | [] -> 0
  | (_ :: xs) -> 1 + length xs


