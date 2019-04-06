(*SORTOWANIE TOPOLOGICZNE*)

(* Autor:  Mateusz Blajda  *)
(* Review: Lukasz Kaminski *)

open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne
(** 
    val topol : ('a * 'a list) list -> 'a list

    Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il 
 *)
 let topol g = 

 (* inicjalizacja wartości*)

 (* funkcja wypełniająca mapę zarami
    (które reprezentować będą ilość krawędzi wchodzących)
     oraz listami krawędzi wychodzących *)
    let mapowanieListy = 
    	List.fold_left (fun acc (a, lista) -> 
        let newLista = if mem a acc then lista@(snd (find a acc)) else lista
        in (add  a (0 , newLista) acc) )
             (empty) g
 (* funkcja która dla każdego sąsiada() elementu o danym indeksie [numer] zmodyfikuje
    ilość krawędzi wchodzących o + [roznica]  *)
    and aktualizujSasiadow roznica numer (mapa,poczatki) = 
        List.fold_left (fun (accMapa,accPocz) x ->

            let (ilosc, listaSasiadow) =
            if (mem x accMapa) 
                then find x accMapa 
                else (0,[])   in
            let newIlosc = ilosc + roznica in 
            ((add x (newIlosc, listaSasiadow) (remove x accMapa)) , (if newIlosc = 0 then x::accPocz else accPocz))
        )
        (mapa, poczatki) (snd (find numer mapa))
    in 
(*  utworzenie mapy zawierającej dla każdego elementu jego ilość krawędzi wchodzących
    oraz listę krawędzi wychodzących *)
    let mapa = fst (
        foldi 
        ( fun a b acc -> (aktualizujSasiadow 1 a acc))
          mapowanieListy (mapowanieListy, [])
        )
    in

(*  utworzenie listy potencjalnych wierzchołków które możemy dodać do listy wynikowej
    tzn takich do których nie wchodzi żadna krawędź  *)
    let poczatki = 
        foldi (fun a (bn,blista) acc -> 
             if bn = 0 then a::acc else acc )
         mapa []
    in
(*  ilość elementów *)
    let n = fold (fun b acc -> acc+1) mapa 0 in 

(*  właściwe liczenie wartości  *)

(*  funkcja która dla danej listy początków, mapy i tablicy wynikowej zwraca
    tablice wynikową *)
    let rec iter poczatki mapa wynik =

     	match poczatki with
    	| []   -> if (List.length wynik) = n then wynik (*wszystkie elementy są w wynik*)
    			  else raise(Cykliczne) (* nie ma wierzchołków bez krawędzi wchodzących,
                                           a nie wszystkie są w wynik - GRAF MA CYKL*)
    	| h::t -> 
    		let (newMapa, newPoczatki) = aktualizujSasiadow (-1) h (mapa,t) (*usuwanie krawędzi i aktualizacja
                                                                            mapy i listy początków*)
    		in iter newPoczatki newMapa (h::wynik) (*dodanie h do wyniku*)

    in List.rev (iter poczatki mapa []) (*odwrócenie wyniku*)

(*
    złożoność czasowa: liniowa względem liczby krawędzi - każda krawędź będzie usunięta raz oraz dodana raz
    złożoność pamięciowa: liniowa względem liczby krawędzi - tyle wymaga mapa zawierająca wszystkie krawędzie
*)
