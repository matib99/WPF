(*******************************)
(** Autor: Mateusz Błajda      *)
(** Reviewer: Piotr Kowalewski *)
(*******************************)

module Origami = struct

(** Punkt na płaszczyźnie *)
type point = float*float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym
punkcie *)
type kartka = point -> int 

(*epsilon jaki jest każdy widzi*)
let epsilon = 1e-15
 

(*porównywanie floatów z dokładnością do epsilona*)
let (=.) a b = (abs_float (a -. b)) < epsilon
let (>=.) a b = (a -. b) > (-. epsilon)
let (>.) a b = (a -. b) > epsilon
let (<=.) a b = (b -. a) > (-. epsilon)
let (<.) a b = (b -. a) > epsilon

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
prostokąt o bokach równoległych do osi układu współrzędnych i lewym
dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat p1 p2 = 
	function
	|(x,y) -> 
	if (x >=. (fst p1) && x <=. (fst p2)) 
	&& (y >=. (snd p1) && y <=. (snd p2))
	then 1 else 0

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
w punkcie [p] i promieniu [r] *)
let kolko p r = 
	function
	|(x,y) ->
	let x0 = x -. (fst p)
	and y0 = y -. (snd p)
	in if (x0*.x0 +. y0*.y0) <=. r*.r 
	then 1 else 0 

(*odejmowanie dwóch punktów*)
let minus p1 p2 =
	match (p1,p2) with 
	| ((x1,y1),(x2,y2)) ->
	(x1-.x2,y1-.y2)

(*obliczanie punktu symetrycznego do [p] względem prostej ([p1],[p2])*)
let symetryczny p1 p2 p = 
	let s = minus p2 p1
	and pp = minus p p1 in
	let (x, y) = pp
	and (x1, y1) = p1
	in
	if (fst s) =. 0.
		then (x1-.x , y+.y1)
		else
		let a = (snd s)/.(fst s) 
		in 
		let nx = x*.((1.-.a*.a)/.(1.+.a*.a)) +.
				 y*.((2.*.a)/.(1.+.a*.a))
		and ny = x*.((2.*.a)/.(1.+.a*.a)) -. 
				 y*.((1.-.a*.a)/.(1.+.a*.a))
		in (nx +. x1, ny +. y1)

(*obliczanie po której stronie prostej ([p1],[p2]) leży punkt [p]
 1 - po lewej   (przed złożeniem)
 0 - na prostej (na złożeniu)
-1 - po prawej  (za złożeniem)
*)
let strona p1 p2 p =
	let (x1,y1) = minus p2 p1 
	and (x2,y2) = minus p  p1 in
	let c = x1*.y2 -. x2*.y1 in
	if c =. 0. then 0 else 
	if c >. 0. then 1 else
	-1

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej
przez punkty [p1] i [p2] (muszą to być różne punkty). Papier jest
składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
od [p1] do [p2]) jest przekładany na lewą. Wynikiem funkcji jest
złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
się na punkt przebicia. *)
let zloz p1 p2 k = fun p -> 
	let c = strona p1 p2 p
	in match c with
	| -1 -> 0
	|  0 -> k p
	|  1 -> (k p) + (k (symetryczny p1 p2 p))
	|  _ -> 42

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych 
z listy *)
let skladaj l k =
	let f = fun acc x -> zloz (fst x) (snd x) acc
	in List.fold_left f k l 

end;;
