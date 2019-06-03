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
  | (x :: xs) -> 
    if k = 1 then Some(x)
    else find_kth (k - 1) xs

(* 1.04 Find the number of elements in a list *)
let length_tailrec lst = 
  let rec aux acc = function
    | [] -> acc
    | (_ :: xs) -> aux (acc + 1) xs
    in aux 0 lst

let rec length = function
  | [] -> 0
  | (_ :: xs) -> 1 + length xs

  (* 1.05 Reverse a list *)
  let reverse_tailrec lst =
    let rec aux reversed = function
      | [] -> reversed
      | (x :: xs) -> aux (x :: reversed) xs
      in aux [] lst

let rec reverse = function
  | [] -> []
  | (x :: xs) -> reverse xs @ [x]

(* 1.06 Find out whether a list is a palindrome *)
let is_palindrome xs =
  let reversed =
    reverse_tailrec xs
  in xs = reversed
