(* ===== Vaja 2: Funkcijsko Programiranje  ===== *)

(*Namig: Napiši si pomožno funkcijo za obračanje seznamov. *)

let reverse l =
  let rec aux_reverse acc l =
    match l with
	| [] -> acc
	| hd::tl -> aux_reverse (hd::acc) tl
  in
  aux_reverse [] l
  
(* Funkcija "repeat x n" vrne seznam n ponovitev x. 
 Za neprimerne n funkcija vrne prazen seznam.
 ----------
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
 ---------- *)

let rec repeat x n = 
  if n <= 0 then [] else x :: (repeat x (n-1))

(* Funkcija "range n" sprejme številio n in vrne seznam vseh celih števil od 0
 do vključno n. Za neprimerne n funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 ----------
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 ---------- *)

let range n = 
  if n<0 then [] 
  else 
    let rec aux_range n acc =
      match n with
        | 0 -> 0::acc
		| n -> aux_range (n-1) (n::acc)
	in
    aux_range n []
	
let range2 n =
  let rec range2_aux n acc =
    if n < 0 then [] else range2_aux (n-1) (n::acc)
  in
  range2_aux n []
  
let range_not_tailrec n =
  let rec range_from m =
    if m > n then []
	else m :: (range_from (m+1))
  in
  range_from 0

(* Funkcija "map f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 in vrne seznam [f(l0); f(l1); f(l2); ...].
 ----------
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let rec map f l = 
  match l with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

(* Funkcija "map_tlrec" je tail-recursive verzija funkcije map.
 ----------
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map_tlrec f l = 
  let rec map_aux f l acc =
    match l with
	| [] -> reverse acc
	| hd::tl -> map_aux f tl ((f hd) :: acc)
  in
  map_aux f l []
  
let map_tlrec2 f l =
  let rec map_aux2 l res_rev =
    match l with
	| [] -> reverse res_rev
	| x::xs -> map_aux2 xs (f x :: res_rev)
  in
  map_aux2 l []

(* Funkcija "mapi f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 ter vrne seznam [f 0 l0; f 1 l1; f 2 l2; ...].
 ----------
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
 ---------- *)

let mapi f l = 
  let rec mapi_aux f l acc i =
    match l with
      | [] -> reverse acc
	  | hd::tl -> mapi_aux f tl ((f i hd) :: acc) (i+1)
  in
  mapi_aux f l [] 0
  
let mapi2 f l =
  let rec mapi_aux l index =
    match l with
	| [] -> []
	| x::xs -> (f x index) :: (mapi_aux xs (index+1))
  in
  mapi_aux l 0
  
(* Funkcija "zip l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] in
 l2 = [l2_0; l2_1; l2_2; ...] in vrne seznam [(l1_0,l2_0); (l1_1,l2_1); ...].
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Seznama razlicnih dolzin.".
 ---------- *)
 
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | ([], _) -> failwith "Seznama razlicnih dolzin."
  | (_, []) -> failwith "Seznama razlicnih dolzin."
  | (x::xs, y::ys) -> (x, y) :: (zip xs ys)  

(* Funkcija "zip_enum_tlrec l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] 
 in l2 = [l2_0; l2_1; l2_2; ...] in vrne [(0, l1_0, l2_0); (1, l1_1, l2_1); ...].
 Funkcija je repno rekurzivna.
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip_enum_tlrec ["a"; "b"; "c"; "d"] [7; 3; 4; 2];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4); (3, "d", 2)]
 ---------- *)

let zip_enum_tlrec l1 l2 = 
  let rec zip_enum_tlrec_aux l1 l2 acc i =
    match (l1, l2) with
	  |([],[]) -> acc
	  |(_, []) | ([], _) -> failwith "Seznama razlicnih dolzin."
	  |(hd1::tl1, hd2::tl2) -> (i, hd1, hd2) :: zip_enum_tlrec_aux tl1 tl2 acc (i+1)
  in
  zip_enum_tlrec_aux l1 l2 [] 0
  
let zip_enum_tlrec2 l1 l2 =
  let rec zip_enum_aux l1 l2 index acc_rev =
    match (l1, l2) with
	| ([], []) -> reverse acc_rev
	| ([], _) | (_, []) -> failwith "Seznama razlicnih dolzin."
	| (x::xs, y::ys) ->
	  let new_head = (index, x, y) in
	  zip_enum_aux xs ys (index+1) (new_head::acc_rev)
	in
	zip_enum_aux l1 l2 0 []

(* Funkcija "unzip l" sprejme seznam l = [(a0, b0); (a1, b2); ...]
 in vrne dvojico seznamov ([a0; a1; ...], [b0; b1; ...]).
 ----------
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let rec unzip l = 
  match l with
  | [] -> ([], [])
  | (x1, x2)::xs ->
    let (l1, l2) = unzip xs in
	(x1::l1, x2::l2)

(* Funkcija "unzip_tlrec l" je tail-recursive verzija funkcije unzip.
 ----------
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip_tlrec l = 
  let rec unzip_tlrec_aux l acc_x acc_y =
    match l with
	| [] -> (reverse acc_x, reverse acc_y)
	| (x, y)::tl -> unzip_tlrec_aux tl (x::acc_x) (y::acc_y)
  in
  unzip_tlrec_aux l [] []

(* Funkcija "fold_left_no_acc f l" sprejme seznam l = [l0; l1; l2; ...; ln] in funkcijo f,
 vrne pa f(... (f (f (f l0 l1) l2) l3) ... ln).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 ----------
 # fold_left_no_acc (^) ["F"; "I"; "K"; "U"; "S"];;
 - : string = "FIKUS"
 ---------- *)

let rec fold_left_no_acc f l = 
  match l with
  | _::[] | [] -> failwith "Prekratek seznam."
  | x::y::[] -> f x y
  | x::y::tl -> fold_left_no_acc f ((f x y) :: tl)

(* Funkcija "apply_sequence f x n" vrne seznam zaporednih uporab funkcije f na x,
 [x; f x; f (f x); ...; f uporabljena n-krat na x].
 Funkcija je repno rekurzivna.
 ----------
 # apply_sequence (fun x -> x*x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x*x) 2 (-5);;
 - : int list = []
 ---------- *)

let apply_sequence f x n = 
  let rec apply_sequence_aux f x n acc =
    if n < 0 
	then reverse acc
	else apply_sequence_aux f (f x) (n-1) (x::acc)
  in
  apply_sequence_aux f x n []

(* Funkcija "filter f l" sprejme seznam l in vrne seznam elementov l,
 za katere funkcija f vrne true.
 ----------
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
 ---------- *)

let rec filter f l =
  match l with
  | [] -> []
  | x::xs -> if f x then x :: (filter f xs) else (filter f xs)

(* Funkcija "exists f l" sprejme seznam l in vrne true če obstaja 
element
 seznama l, za katerega fukcija f vrne true in false sicer.
 Funkcija je repno rekurzivna.
 ----------
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
 ---------- *)

let rec exists f l = 
  match l with
  | [] -> false
  | x::xs -> if f x then true else exists f xs
  
let rec exists f = function
  | [] -> false
  | x::xs -> f x || exists f xs

(* Funkcija "first f none_value l" sprejme seznam l in vrne prvi element seznama,
 za katerega funkcija f vrne true, če takšnega elementa ni, pa vrne none_value.
 Funkcija je repno rekurzivna.
 ----------
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
 ---------- *)

let rec first f none_value = function
  | [] -> none_value
  | x::xs -> if f x then x else first f none_value xs
  
(* Severnjaki napadajo Medbrezje. Kot vrhovni čarodej poznaš zaporedje urokov s katerimi
 lahko Medbrezje zaščitiš pred napadom, zaporedje urokov pa je predstavljeno v seznamu oblike
 [("ime1", vrednost1); ("ime2", vrednost2); ...].
 Na razpolago imaš skupino čarodejov, ki so prav tako predstavljeni v seznamu oblike
 [("ime1", spretnost1); ("ime2", spretnost2); ...].
 Čarodej lahko izvede zaporedje urokov, če je njegova spretnost večja ali enaka skupni
 vrednosti vseh urokov v zaporedju.

 Funkcija "able_protectors spells wizards" vrne seznam imen vseh čarodejov, ki lahko
 samostojno zaščitijo Medbrezje.


 Funkcija "fails_on spells wizards" vrne seznam parov (čarodej, neuspešni urok), kjer
 je neuspešni urok prvi urok v zaporedju, za katerega čarodej nima več dovolj spretnosti.
 Če lahko čarodej zaporedje izvede v celoti, to predstavlja prazen niz.

 Namig: Dober čarodej uporablja svoje znanje in izkušnje, ki jih pridobi tekom učenja.

 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Atijam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   able_protectors spells wizards;;
 - : string list = ["Merlin"; "Atijam"]
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Atijam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   fails_on spells wizards;;
 - : (string * string) list = [("Merlin", ""); ("Frodo", "Renounce"); ("Atijam", "");
  ("Mr Duck", "Protect"); ("Kylo Ren", "Banish"); ("Snoop Dogg", "Blaze")]
 ----------*)

let able_protectors spells wizards = 
  (* Unzip to sum all costs. *)
  let (_, spell_values) = unzip spells in
  (* Sum the costs. 0::0::spell_values added so that an empty list sums to 0. *)
  let sequence_cost = fold_left_no_acc (+) (0::0::spell_values) in
  (* Make filter function. *)
  let can_cast (_, wizard_ability) = (wizard_ability >= sequence_cost) in
  (* Get only the wizards who can cast. *)
  let mighty_wizards = filter can_cast wizards in
  (* Extract names. *)
  let (mighty_wizard_names, _) = unzip_tlrec mighty_wizards in
  mighty_wizard_names

let fails_on spells wizards = 
  (* Write auxiliary function that determines the first uncastable spell. *)
  let rec gets_stuck spells wizard_ability =
    match spells with
	| [] -> ""
	| (spell_name, spell_value)::tl ->
	  if wizard_ability >= spell_value
	  then gets_stuck tl (wizard_ability - spell_value)
	  else spell_name
  in
  (* Unzip to get a list of wizzard ability values. *)
  let (wizard_names, wizard_abilities) = unzip wizards in
  (* Create a list of first uncastable spells. *)
  let stuck_spells = map (gets_stuck spells wizard_ability) wizard_abilities in
  (* Zip it back together. *)
  zip wizard_names stuck_spells