(* 1.NALOGA *)

let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> print_int x; izpisi_vsa_stevila xs

let map2_opt f xs ys =
  let rec aux acc = function
    | ([], []) -> Some (List.rev acc)
    | (_, []) | ([], _) -> None
    | (x :: xs, y :: ys) -> aux ((f x y) :: acc) (xs, ys)
  in
  aux [] (xs, ys)

(* 2.NALOGA *)

type 'a filter_tree =
  | List of 'a list
  | Vozlisce of 'a filter_tree * 'a * 'a filter_tree

let test_tree = 
  let levo = Vozlisce(List ([1]), 5, List([])) in
  let desno = Vozlisce(List ([]), 15, List([19; 20])) in
  Vozlisce(levo, 10, desno)

let rec vstavi n = function
  | List xs -> List (n :: xs)
  | Vozlisce (l, x, d) when n <= x -> Vozlisce (vstavi n l, x, d)
  | Vozlisce (l, x, d) -> Vozlisce(l, x, vstavi n d)

let rec vstavi_seznam xs ft =
  match xs with
    | [] -> ft
    | x :: xs -> vstavi_seznam xs (vstavi x ft)

let vstavi_seznam2 xs ft =
  List.fold_right vstavi xs ft

let rec preveri_manjsi_ali_enak n = function
  | [] -> true
  | x :: xs when x > n -> false
  | x :: xs -> preveri_manjsi_ali_enak n xs

let rec preveri_vecji n = function
  | [] -> true
  | x :: xs when x > n -> preveri_vecji n xs
  | x :: xs -> false

let rec preveri_drevo = function
  | List _ -> true (* primer ko je drevo samo seznam *)
  | Vozlisce(List xs, n, List ys) -> 
    (preveri_manjsi_ali_enak n xs) && (preveri_vecji n ys)
  | Vozlisce(List xs, n, r) ->
    (preveri_manjsi_ali_enak n xs) && (preveri_drevo r)
  | Vozlisce(l, n, List ys) ->
    (preveri_vecji n ys) && preveri_drevo l
  | Vozlisce(l, _, r) ->
    (preveri_drevo l) && (preveri_drevo r)

let rec pravilno ft =
  let cheker lower upper x =
    match (lower, upper) with
      | (None, None) -> true
      | (Some l, None) -> x > l
      | (None, Some u) -> x < u
      | (Some l, Some u) -> x > l && x < l
  in
  let rec vrednosti_med lower upper = function
    | List xs -> List.for_all (cheker lower upper) xs
    | Vozlisce (l, n, d) ->
      (vrednosti_med lower (Some n) l) && (vrednosti_med (Some n) upper d)
  in
  vrednosti_med None None ft

(* let rec boxed_correctly ftree =
  let checker lower upper x =
    match (lower, upper) with
    | (None, None) -> true
    | (Some l, None) -> l <= x
    | (None, Some u) -> x < u
    | (Some l, Some u) -> l <= x && x < u
  in
  let rec values_between lower upper ftree =
    match ftree with
    | Box(xs) -> List.for_all (checker lower upper) xs
    | Node(f, lt, rt) ->
      (values_between lower (Some f) lt) && (values_between (Some f) upper rt)
  in
  values_between None None ftree     *)


(* 3.NALOGA *)

type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  type t

  val id : t
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t
  val kompozitum : t -> t -> t
end

module Matrika : Linearna = struct

  type t = matrika

  let id = (1, 0, 0, 1)
  
  let uporabi m v =
    let (a, b, c, d) = m in
    let (x, y) = v in 
    (a * x + b * y, c * x + d * y)
  
  let iz_matrike m = m
  
  let iz_funkcije f = 
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  
  let kompozitum m1 m2 =
    let (a, b, c, d) = m1 in
    let (x, y, z, w) = m2 in
    let prva = a * x + b * z in
    let druga = a * y + b * w in
    let tretja = c * x + d * z in
    let cetrta = c * y + d * w in
    (prva, druga, tretja, cetrta)

end

module Funkcija : Linearna = struct

  type t = vektor -> vektor

  let id = (fun x -> x)

  let uporabi p v = p v

  let iz_matrike m = 
    let (a, b, c, d) = m in
    fun (x, y) -> (a * x + b * y, c * x + d * y)

  let iz_funkcije f = f

  let kompozitum f g = fun x -> f (g x)
end

(* 4.NALOGA *)

(* V pythonu *)