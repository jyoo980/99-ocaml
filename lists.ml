(* map function *)
let map f lst =
  let rec aux acc = function
    | [] -> acc
    | (x :: xs) -> aux (acc @ [f x]) xs
    in aux [] lst

(* Head-recursive filter *)
let rec filter f xs =
  match xs with
    | [] -> []
    | h :: t -> if f h then h :: filter f t else filter f t

(* Tail-recursive filter *)
let filter_tailrec f lst =
  let rec aux acc = function
    | [] -> acc
    | (x :: xs) -> 
      if f x then
        aux (acc @ [x]) xs 
      else 
        aux acc xs
    in aux [] lst

(* Right-fold function *)
let rec foldr f b xs =
  match xs with
    | [] -> b
    | h :: t -> f h (foldr f b t)

(* Left-fold function *)
let foldl f b lst =
  let rec aux acc = function
    | [] -> acc
    | (x :: xs) -> aux (f x acc) xs
    in aux b lst

let range i j =
  let rec aux acc n = 
    if n < i then acc else aux (n :: acc) (n - 1)
  in aux [] j

let build_range f i j =
  map f (range i i)

let fib n =
  let rec aux prev curr = function
    | 0 -> prev
    | 1 -> curr
    | x -> aux curr (prev + curr) (x - 1)
  in aux 0 1 n