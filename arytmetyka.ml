
(****************************************)
(*          Arytmetyka.ml               *)
(*                                      *)          
(*       Mateusz Błajda (mb406098)      *)
(*                                      *)
(****************************************)


(*zdefiniowanie typu wartosc jako przedział lub też jego dopełnienie,*)
(*reprezentowane przez uporządkowaną parę floatów, gdzie lewy float*)
(* oznacza minimalną wartość prawy zaś maksymalną.*)
(* Jeżeli lewy > prawy to oznacza, że przedział jest *)
(*dopełnieniem przedziału (prawy,lewy) czyli wszystkimi*)
(*liczbami rzeczywistymi nie znajdującymi się w przedziale (prawy, lewy)*)
type wartosc = (float * float);; 

(*konstruktor typu wartość dla danej wartości oczekiwanej i niedokładności*)                                                                
let wartosc_dokladnosc (x : float) (p : float) =                                                
    let l = (x -. (x *. p) /. 100.0)                                                            
    and r = (x +. (x *. p) /. 100.0)                                                            
in if(l<=r) then ((l , r):wartosc)
else ((r , l):wartosc);;        


(*konstruktor typu wartość dla danych minimmalnej i maksymalnej wartości *)
let wartosc_od_do (x : float)  (y : float)  = ((x , y) : wartosc);;     

(*konstruktor typu wartość dla danej wartości dokładnej*)                       
let wartosc_dokladna (x : float) = ((x , x) : wartosc);;                                        

(*funkcja zwracająca lewego floata opisującego wartość*)
let lewa_wartosc (w:wartosc) =                                  
    match w with
    |(a,_)->a
    (*|w -> nan;;*)
(*funkcja zwracająca prawego floata opisującego wartość*)
let prawa_wartosc (w:wartosc) =                                 
    match w with
    |(_,b)->b
    (*|w -> nan;;*)

(*funkcja zwracająca najmniejszą wartość w przedziale*)
let min_wartosc (w:wartosc) =                                   
    match w with
    |(a,b)->  if a = a && b = b then 
                if b>=a then a 
                else neg_infinity
            else nan
    (*|_->nan*)
    
(*funkcja zwracająca największą wartość w przedziale*)
let max_wartosc (w:wartosc) =                                   
    match w with
    |(a,b)-> if a = a && b = b then 
                if b>=a then b 
                else infinity
            else nan
    (*|_->nan*)
    

(*funkcja zwracająca średnią wartość w przedziale*)
let sr_wartosc (w:wartosc) =                                    
    ((max_wartosc w) +. (min_wartosc w))/. 2.;;

(*pomocnicze wartości reprezentujące kolejno cały zbiór wszystkich liczb rzeczywistych*)
(*zbiór liczb rzeczywistych nieujemnych oraz niedodatnich*)
let w_inf = wartosc_od_do neg_infinity infinity;;   
let w_0inf = wartosc_od_do 0. infinity;;
let w_inf0 = wartosc_od_do neg_infinity 0.;;

(*funkcja zwracająca wartość (0,0) - w; funkcja ta służy do odejmowania*)
let przeciwna (w : wartosc) =
    wartosc_od_do (0.0 -. prawa_wartosc w ) (0.0 -. lewa_wartosc w);;
(*funkcja zwracająca dopełnienie przedziału tzn wartość 
reprezentującą te i tylko te liczby które nie znajdują się w wartości w*)       
let dopelnienie (w : wartosc) = 
    wartosc_od_do (prawa_wartosc w) (lewa_wartosc w);;      
(*funkcja sprawdzająca czy wartość jest zwykłym 
przedziałem czy przedziałem dopełnionym*)               
let dopelniony (w:wartosc) = ((lewa_wartosc w) > prawa_wartosc w);;                                     

(*funkcja sprawdzająca, czy dana liczba x zawiera się
w przedziale reprezentowanym przez wartosć w*)
let in_wartosc (w : wartosc) (x:float) =                        
    if  min_wartosc w != min_wartosc w then false else          
    if not (dopelniony w) then
    (x>= lewa_wartosc w)&&(x<=prawa_wartosc w) 
    else
    (x>=lewa_wartosc w)||(x<=prawa_wartosc w);;



(*funkcja dodająca do siebie 2 wartości*)
let plus (w1 : wartosc) (w2 : wartosc) =    
	if classify_float (lewa_wartosc w1) = FP_nan || classify_float (lewa_wartosc w2) = FP_nan then wartosc_dokladna nan else                                                                
    if (dopelniony w1 && dopelniony w2) then w_inf  
    else let w3 = wartosc_od_do
                 (lewa_wartosc w1 +. lewa_wartosc w2) 
                 (prawa_wartosc w1 +. prawa_wartosc w2)     
    in if  ((dopelniony w1 || dopelniony w2) && (dopelniony w3 = false ))
    then w_inf else w3;;                            

(*funkcja dodająca do wartości w1 wartość przeciwną do w2 czyli odejmująca w2 od w1*)   
let minus (w1 : wartosc) (w2 : wartosc) =                                                                   
    plus w1 (przeciwna w2);;


(*funkcja pomocnicza mnożąca wartości w1 i w2 zakładając, że w1 jest przedziałem*)
(*dopełnionym natomiast w2 zwykłym oraz że wszystkie wartości należące do w2 są dodatnie*)
let razy_mieszane w1 w2 =                   
    let a1 = lewa_wartosc w1            
    and b1 = prawa_wartosc w1 
    and a2 = lewa_wartosc w2
    and b2 = prawa_wartosc w2
    in 
    let w3 = 
    if(b2 != infinity) 
    then wartosc_od_do (min(a1*.a2) (a1*.b2)  ) (max (b1*.a2) (b1*.b2))
    else 
        if b1 > 0. then w_inf
        else wartosc_od_do (a1*.a2) (b1*.a2) 
        in
        if dopelniony w3  then w3
        else w_inf      
    ;;


(*funkcja zwracająca pewien typ przypadku szczególnego*)
(*istnieje tu tylko dlatego że OCaml matchuje wszytko z -inf*)
let szablon w1 w2 =

let a1 = lewa_wartosc w1
and b1 = prawa_wartosc w1
and a2 = lewa_wartosc w2
and b2 = prawa_wartosc w2
in
let rodzaj a = 
    if(classify_float a = FP_infinite) then if a > 0. then 2 else -2 
	else
    	if(classify_float a = FP_zero)then 0 
		else (1)

in  ((rodzaj a1),(rodzaj b1),(rodzaj a2),rodzaj(b2));;

(*mnożenie dla pewnych szczególnych przypadków brzegowych*)
let razy_szczegolne w1 w2 =         
    let a1 = lewa_wartosc w1
    and b1 = prawa_wartosc w1
    and a2 = lewa_wartosc w2
    and b2 = prawa_wartosc w2

(*wymienienie wszystkich przypadków brzegowych*) 
in match szablon w1 w2 with
|(-2,1,0,1)-> wartosc_od_do (neg_infinity) (max 0. (b1*.b2))
|(-2,1,1,0)-> wartosc_od_do (min 0. (b1*.a2)) (infinity)
|(0,1,-2,1)-> wartosc_od_do (neg_infinity) (max 0. (b2*.b1))
|(1,0,-2,1)-> wartosc_od_do (min 0. (b2*.a1)) (infinity)
|(1,2,0,1)-> wartosc_od_do (min 0. (a1*.b2)) (infinity)
|(1,2,1,0)-> wartosc_od_do (neg_infinity) (max 0. (a1*.a2))
|(0,1,1,2)-> wartosc_od_do (min 0. (b1*.a2)) (infinity)
|(1,0,1,2)-> wartosc_od_do (neg_infinity) (max 0. (a1*.a2)) 

|(0,2,1,0)-> w_inf0
|(0,2,0,1)-> w_0inf
|(-2,0,1,0)->w_0inf
|(-2,0,0,1)->w_inf0
|(1,0,0,2)->w_inf0
|(0,1,0,2)->w_0inf
|(1,0,-2,0)->w_0inf
|(0,1,-2,0)->w_inf0

|(-2,0,1,1)-> if(in_wartosc w2 0.) then w_inf else w_inf0
|(0,2,1,1)-> if(in_wartosc w2 0.) then w_inf else w_0inf
|(1,1,-2,0)-> if(in_wartosc w1 0.) then w_inf else w_inf0
|(1,1,0,2)-> if(in_wartosc w1 0.) then w_inf else w_0inf

|(0,2,0,2)-> w_0inf
|(-2,0,-2,0)->w_0inf
|(-2,0,0,2)->w_inf0
|(0,2,-2,0)->w_inf0
|(0,0,_,_)->wartosc_dokladna 0. 
|(_,_,0,0)->wartosc_dokladna 0. 
|(-2,2,_,_)->w_inf
|(_,_,-2,2)->w_inf
|_->wartosc_dokladna (nan)
    ;;

(*pomocnicze funkcje minimum i maksimum*)
let min4 a b c d = min (min a b) (min c d);;
let max4 a b c d = max (max a b) (max c d);;

(*funkcja sprawdzająca, w1 * w2 to przypadek szczególny*)
let szczegolny w1 w2 = 
    let a1 = lewa_wartosc w1
    and b1 = prawa_wartosc w1
    and a2 = lewa_wartosc w2
    and b2 = prawa_wartosc w2
in
    let il1 = a1*.a1*.b1*.b1
    and il2 = a2*.a2*.b2*.b2
    (*jeżeli il1 = 0 a il2 = +-inf to il1*il2 = nan*)
    (*jeżeli np a1 = 0 a b1 = inf to il1*il2 też da nan*)
    (*w ten sposób rozpoznaje przypadki szczególne*)
in classify_float (il1*.il2) = FP_nan;;                          


(*funkcja mnożąca przez siebie 2 wartości*)
let razy (w1 : wartosc) (w2 : wartosc) = 

    if(classify_float (lewa_wartosc w1) = FP_nan)||(classify_float (lewa_wartosc w2) = FP_nan) 
    then wartosc_dokladna nan else
    if (w1 = (wartosc_dokladna 0. ))||( w2 = (wartosc_dokladna 0.)) 
    then wartosc_dokladna 0. else   

    let a1 = lewa_wartosc w1
    and b1 = prawa_wartosc w1
    and a2 = lewa_wartosc w2
    and b2 = prawa_wartosc w2
    
    in 
          let dop = (dopelniony w1 , dopelniony w2)
          in 
          match dop with 
          (*dwie wartości zwykłe*)
          |(false,false) -> 
            if szczegolny w1 w2 then razy_szczegolne w1 w2 
            else  wartosc_od_do  
            (min4 (a1*.a2) (a1*.b2) (b1*.a2) (b1*.b2)) 
            (max4 (a1*.a2) (a1*.b2) (b1*.a2) (b1*.b2)) 
          (*jedna zwykła druga dopełniona*)
          |(false,true) -> 
            if in_wartosc w1 0.0 then w_inf                                                                         
            else if a1>0.0 then razy_mieszane w2 w1                                                         
            else przeciwna (razy_mieszane w2 (przeciwna w1))       
          (*jedna dopełniona druga zwykła*) 
          |(true,false) ->  
            if in_wartosc w2 0.0 then w_inf
            else if a2>0.0 then razy_mieszane w1 w2
            else przeciwna (razy_mieszane w1 (przeciwna w2)) 
          (*obie dopełnione*)
          |(true,true) -> 
            if((in_wartosc w1 0.0 )||( in_wartosc w2 0.0)) then w_inf    
            else wartosc_od_do (min (a1*.a2) (b1*.b2)) (max (b1*.a2) (a1*.b2))
    ;;

(*funkcja zwracająca odwrotność wartości w*)
(*to znaczy (1,1)/w *)
let odwrotnosc w:wartosc = 
	if classify_float (lewa_wartosc w) = FP_nan then wartosc_dokladna nan else
    if w = w_inf then w_inf else                                                                                
    match w with
    |(0.0 , 0.0) -> wartosc_od_do nan nan
    |(0.0 , b) -> wartosc_od_do (1.0 /. b)  (if b > 0.0 then infinity else neg_infinity)
    |(a , 0.0) -> wartosc_od_do ( if a > 0.0 then infinity else neg_infinity)  (1.0 /. a)
    |(a , b) -> wartosc_od_do (1.0 /. b)  (1.0 /. a);;

(*funkcja mnożąca wartość w1 przez odwrotność wartości w2 czyli dzieląca w1 przez w2*)
let podzielic (w1:wartosc) (w2:wartosc) =                                                               
    razy w1 (odwrotnosc w2);;


