(* 1. NALOGA *)

(* a *)
let uporabi f x = f x

(* b *)
let ibaroupu x f = f x

(* c *)
let zacetnih n xs = 
  let rec aux acc n xs =
    match (xs, n) with
    | ([], n) when n > 0 -> None
    | (_, 0) -> Some (List.rev acc)
    | (x :: xs, n) -> aux (x :: acc) (n-1) xs
  in
  aux [] n xs

let rec drop n xs =
  if n <= 0 then
    Some xs
  else
    match xs with
    | [] -> None
    | _ :: xs -> drop (n-1) xs

(* 2. NALOGA *)

type 'a neprazen_sez =
  | Konec of 'a
  | Sestavljen of 'a * 'a neprazen_sez

let test_seznam = (Sestavljen (1, Sestavljen (2, Sestavljen (3, Konec (4)))))


(* a *)

let prvi = function
  | Konec (a) -> a
  | Sestavljen (a, _) -> a
 
let rec zadnji = function 
  | Konec (a) -> a
  | Sestavljen (_, xs) -> zadnji xs

(* b *)

let dolzina xs=
  let rec aux acc = function
  | Konec (_) -> acc + 1
  | Sestavljen (_, xs) -> aux (acc+1) xs
  in
aux 0 xs

(* c *)

let pretvori_v_seznam xs =
  let rec aux acc = function
  | Konec (x) -> List.rev (x :: acc)
  | Sestavljen (x, xs) -> aux (x :: acc) xs
  in
  aux [] xs

(* d *)

let rec zlozi f = function
  | Konec (x) -> Konec(f x)
  | Sestavljen (x, xs) -> 
    let preslikan = f x in
    Sestavljen (preslikan,  zlozi f xs)


(* 3.NALOGA *)

(* a *)

let simetricen str =
  let obrni niz =
    let dolzina = String.length niz in
    String.init dolzina (fun i -> niz.[dolzina - 1 - i])
    (* let len = String.length niz in
    let res = String.create len in 
    let last = len - 1 in
    for i = 0 to last do
      let j = last - i in
      res.[i] <- niz.[j];
    done;
    res *)
  in
  let obrnjen = obrni str in
  str = obrnjen
