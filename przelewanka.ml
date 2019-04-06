(**
	Przelewanka : 
	Autor: Mateusz Błajda
	Reviewer: Jan Jurkowski
*)


(*liczy nwd a i b*)
let rec nwd a b = 
	if b = 0 
	then a
else if a < b 
	 then nwd b a
	 else nwd (a mod b) b

(*liczy nwd całej tablicy*)
let nwd_Array arr = 
	let n = Array.length arr 
	and nwd_t = ref arr.(0) in
	for i = 1 to (n-1) do
		nwd_t := nwd (!nwd_t) (arr.(i));
	done;
	!nwd_t

(*dzieli tablice (int * int) na 2 tablice int *)
let rozdziel arr = 
	let n = Array.length arr in
	let wynik = Array.make n 0
	and pojemnosc = Array.make n 0 in

	for i = 0 to n-1 do
		wynik.(i) <- (snd arr.(i));
		pojemnosc.(i) <- (fst arr.(i));
	done;
	(wynik,pojemnosc)

(*sprawdza wstępnie czy wynik jest możliwy do otrzymania
tzn odrzuce te dane wejściowe, w których NWD tablicy z wynikami jest
podzielne przez NWD tablicy z pojemnościami - wtedy na pewno się nie da*)
let checkNWD pojemnosc wynik =  
	let nwdP = nwd_Array pojemnosc
	and nwdW = nwd_Array wynik in
	nwdW mod nwdP = 0	
(*Sprawdza czy chociaż jedno naczynie ma byc na koncu puste bądź pełne*)
let checkW pojemnosc wynik = 
	let n = Array.length wynik in
	let czy = ref false in
	for i=0 to (n-1) do
		if (wynik.(i) = 0) || (wynik.(i) = pojemnosc.(i))
		then czy := true
	done;
	!czy
(*sprawdza obie powyższe rzeczy*)
let check pojemnosc wynik = 
 	(checkW pojemnosc wynik)&&(checkNWD pojemnosc wynik)
(*wyrzuca z tablicy wszystkie pary (0,0) gdyż nie zmieniają one w żaden 
sposób niczego*)
let napraw arr = 
	let n = Array.length arr in
	let lista = ref[] in
	for i = 0 to n-1 do
		if arr.(i) <> (0,0) then 
		lista := (arr.(i)) :: !lista;
	done;
	Array.of_list !lista


(** zwraca wynik zadania dla tablicy arrrr*)
let przelewanka arrrr = 

	let arr = napraw arrrr in
	if Array.length arr = 0 then 0 else
	let n = Array.length arr 
	and (wynik,pojemnosc) = rozdziel arr 
	and ht = Hashtbl.create 420000 in 

	if not (check pojemnosc wynik)
	then -1
	else


	let kolejka = Queue.create() in (*kolejka do której wrzucam kolejne *)
	begin                                 (*możliwe do uzyskania stany naczyń*)
		let poczatek = Array.make n 0 in
		Queue.add poczatek kolejka;
		Hashtbl.add ht poczatek 0;
	end;

	let czy = ref false in 

	while  not (!czy || (Queue.is_empty kolejka))  do 

		let stan = Queue.pop kolejka in 
		if (stan = wynik)
		then czy :=  true
		else czy :=  false (*czy wynik jest już uzyskany?*)
;
		if not !czy then

		let ruchy = Hashtbl.find ht stan in
		begin 

			(*wylewanie*)
			for i = 0 to (n-1) do
				if stan.(i) <> 0 then
				let newStan = Array.copy stan in
				newStan.(i) <- 0;
				if not (Hashtbl.mem ht newStan) 
				then 
					begin
						Queue.add newStan kolejka;
						Hashtbl.add ht newStan (ruchy+1);
					end;

			done;

			(*nalewanie*)
			for i = 0 to (n-1) do
				if stan.(i) <> pojemnosc.(i) then
				let newStan = Array.copy stan in
				newStan.(i) <- pojemnosc.(i);
				if not (Hashtbl.mem ht newStan) 
				then 
					begin
						Queue.add newStan kolejka;
						Hashtbl.add ht newStan (ruchy+1);
					end;
			done;

			(*przelewanie*)
			for i = 0 to (n-1) do
				for j = 0 to (n-1) do
					let newStan = Array.copy stan in
					if not ((i = j) || (newStan.(i) = 0) || (newStan.(j) = pojemnosc.(j))) then
						(*przelewam z I do J*)
						let doPelna = pojemnosc.(j) - stan.(j) in
						let doLewka = min doPelna (stan.(i)) in
						begin
							newStan.(j) <- (newStan.(j) + doLewka);
							newStan.(i) <- (newStan.(i) - doLewka);
						end;
						if not (Hashtbl.mem ht newStan) 
							then 
								begin
									Queue.add newStan kolejka;
									Hashtbl.add ht newStan (ruchy+1);
								end;
				done;
			done;
		end;
	done;

	
(*jak mamy wynik to daj wyink a jak nie to -1*)
if Hashtbl.mem ht wynik then 

	Hashtbl.find ht wynik
else
   (-1)


