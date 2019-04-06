(*Autor: Mateusz Błajda 406098*)
(*Code reviewer: Jan Jurkowski*)


(** Typ złączalnej kolejki priorytetowej 
	każdy niepusty wierzchołek zawiera kolejno :
	lewego syna, priorytet(wartość), długość 
	skrajnie prawej ścieżki, prawego syna*)
type 'a queue = None | Node of 'a queue * 'a * int * 'a queue

(** Pusta kolejka priorytetowa *)
let empty = None

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)
let is_empty q = (q = empty)

(*funkcje pomocnicze zwracające poszczególne wartości z krotki*)
let left q = 
	match q with
	| Node (l,_,_,_) -> l
	| None			 -> None
let right q = 
	match q with
	| Node (_,_,_,r) -> r
	| None     		 -> None
let priority q = 
	match q with
	| Node (_,p,_,_) -> p
	| None      	 -> max_int
let r_lenght q =
	match q with
	| Node (_,_,rl,_) -> rl
	| None       	  -> -1


(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 = 
	if(is_empty q1) then q2 else
	if(is_empty q2) then q1 else 
	if(priority q1) > (priority q2) 
	then join q2 q1 														(*złączenie łamałoby warunek kopca więc zamieniamy argumenty*)
	else
	let q3 = join (right q1) q2												
	in 
	if (r_lenght q3) > (r_lenght (left q1)) 								(*warunek lewicowości - drzewo z dłuższą skrajnie prawą ścieżką ląduje po lewej stronie*)
	then Node (q3, priority q1, (r_lenght (left q1)) + 1, left q1)			
	else Node (left q1, priority q1, (r_lenght q3) + 1, q3)

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)
let add a q = 
	join q (Node (empty, a, 0, empty))

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q = 
	if is_empty q then raise Empty
	else (priority q, join (left q) (right q))





	
