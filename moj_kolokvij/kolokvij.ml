(* -------- 1 -------- *)

let rec vsota_seznama xs = 
  let rec arg acc = function
    | [] -> acc
    | x :: xs -> arg (x + acc) xs
  in
  arg 0 xs

(* -------- 2 -------- *)

let rec urejen xs =
  match xs with
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> 
    if x > y
    then false
    else urejen (y :: xs)


(* -------- 3 -------- *)

let rec vstavi x ys =
  match ys with
  | [] -> x :: []
  | y :: ys -> 
    if (y > x)
    then ( x :: y :: ys)
    else y :: vstavi x ys

let rec preuredi xs =
  let rec arg acc = function
  | [] -> acc
  | x :: xs -> arg (vstavi x acc) xs
  in
  arg [] xs

(* -------- 4 -------- *)
let rec vstavi' cmp x ys =
  match ys with
  | [] -> [x]
  | y :: ys ->
    if cmp x y 
    then ( x :: y :: ys)
    else y :: vstavi' cmp x ys

let rec urejanje' xs cmp = 
  let rec arg acc = function
  | [] -> acc
  | x :: xs -> arg (vstavi' cmp x acc) xs
  in
  arg [] xs

(* -------- 5 -------- *)

type priority = 
  | Top
  | Group of int

type status = 
  | Staff 
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)



(* -------- 7 -------- *)

