(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree


(* alternativa
type 'a tree =
    | Empty
    | Leaf 'a
    | Node of 'a tree * 'a * 'a tree 
    
Leaf x = Node (Empty, x, Empty)
*)

let leaf x = Node (Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree =
    let left_t = Node (leaf 0, 2, Empty) in
    let right_t = Node (leaf 6, 7, leaf 11) in
    Node(left_t, 5, right_t)


(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
    | Empty -> Empty
    | Node (lt, x, rt) -> Node (mirror rt, x, mirror lt)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec size = function
    | Empty -> 0
    | Node (lt, x, rt) -> 1 + size lt + size rt

(* Repno rekurzivna funkcija "size": *)

let tl_rec_size tree =
    let rec size' acc queue =
        (* Pogledamo, kateri je nasledni element v vrsti za obravnavo. *)
        match queue with
        | [] -> acc
        | t :: ts -> (
            (* Obravnavamo drevo. *)
            match t with
            | Empty -> size' acc ts (* Prazno drevo samo odstranimo iz vrstice. *)
            | Node (lt, x, rt) -> 
                let new_acc = acc + 1 in (* Obravnavamo vozlišče. *)
                let new_queue = lt :: rt :: queue in (* Dodamo poddrevesa v vrsto. *)
                size' new_acc new_queue
        )
    in
    size' 0 [tree]

let rec height = function
    | Empty -> 0
    | Node (lt, x, rt) -> 1 + max (height lt) (height rt)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree =
    match tree with 
    | Empty -> Empty
    | Node (lt, x, rt) -> Node (map_tree f lt, f x, map_tree f rt)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
    | Empty -> []
    | Node (lt, x, rt) -> (list_of_tree lt) @ [x] @ (list_of_tree rt)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_bst = function
    | Empty -> true
    | Node (lt, x, rt) ->
    let l_sez = list_of_tree lt in
    let d_sez = list_of_tree rt in
    (x = List.fold_left max x l_sez) && 
    (x = List.fold_left min x d_sez) && 
    (is_bst lt) && 
    (is_bst rt)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec member a = function
    | Empty -> false
    | Node (lt, x, rt) ->
        if x < a then
            member a lt
        else if x > a then
            member a rt
        else 
            x = a
    
let rec insert a = function
    | Empty -> leaf a
    | Node (lt, x, rt) as drevo ->
        if x > a 
        then Node (insert a lt, x, rt)
        else if x < a
        then Node (lt, x, insert a rt)
        else drevo

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 a = function
    | Empty -> false
    | Node(l, x, d) ->
        if x = a then
            true
        else (member2 a l) || (member2 a d)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ = function
    | Empty -> None
    | Node(Empty, a, Empty) -> None
    | Node(_, a, r) -> let lista = (list_of_tree r) in
        match lista with
        | [] -> None
        | x :: xs -> Some x
     
let pred = function
    | Empty -> None
    | Node(Empty, a, Empty) -> None
    | Node(l, a, _) -> 
    let l = list_of_tree l in
    let rec iskanje_max list = 
        match list with
        | [] -> None
        | x :: [] -> Some x
        | _ :: xs -> iskanje_max xs 
    in
    iskanje_max l

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x = function
	| Empty -> Empty
	| Node(Empty, y, Empty) when x = y -> (* Leaf cases *) Empty
	| Node(Empty, y, rt) when x = y -> (* One sided *) rt 
	| Node (lt, y, Empty) when x = y -> (* One sided *) lt
	| Node(lt, y, rt) when x <> y -> (* Recurse deeper *)
		if x > y then
			Node(lt, y, delete x rt)
		else
			Node(delete x lt, y, rt)
	| Node(lt, y, rt) as drevo -> 
		match succ drevo with
		| None -> failwith " To se ne more zgoditi! "
		| Some z -> Node(lt, z, delete z rt)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(* Alternativa *)
type ('key, 'value) dict =
    | Prazno
    | Sestavljeno of ('key, 'value) dict * ('key * 'value) * ('key, 'value) dict

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = Node(leaf ("a", 0), ("b", 1) , Node(leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
	| Empty -> None
	| Node(ld, (k, v), rd) ->
		if k = key then 
			Some v
		else if k > key then
			dict_get key ld
		else
			dict_get key rd

(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let print_dict dict =
    let list_of_dict = list_of_tree dict in
    let rec pom = function
    | [] -> ()
    | (k, v) :: xs -> 
        print_string(k ^ " : " ^ string_of_int v);
        print_newline ();
        pom xs
    in
    pom list_of_dict

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value = function
    | Empty -> leaf (key, value)
    | Node(l, (k, v), r) ->
        if key = k then
            Node(l, (k, v), r)
        else if key < k then
            Node(dict_insert key value l, (k, v), r)
        else
            Node(l, (k, v), dict_insert key value r)