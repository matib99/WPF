(*
 * ISet - Interval sets
 * Copyright (C) 1996-2018 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Mateusz Błajda
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 
*)

(** [t] - interval set with structure of AVL tree 
    [t] contains left child, interval, right child, height of [t], 
    quantity of numbers in all intervals in set [t] *)
type t =
  | Empty
  | Node of t * (int*int) * t * int * int 

(** The empty set *)
let empty = Empty

(** returns true if the set is empty. *)
let is_empty t = 
  t = Empty

(* [cmp x (minv,maxv)] compares x with interval [minv,maxv] and returns 
     1 when [x] is greater then [maxv],
    -1 when [x] is lesser than [minv],
     0 if [x] <= [maxv] and [x] >= [minv]*)
let cmp x (minv,maxv) = 
     if maxv < x then  1 else
     if minv > x then -1 else 0


(* [safe_positive_addition a1 a2] returns sum of 2 positive integers [a1] and [a2]. 
   If [a1] + [a2] is bigger than max_int then function returns max_int*)
let safe_positive_addition a1 a2 =
  if (max_int - a2) >= a1 then a1 + a2
  else max_int

(* [safe_positive_addition l] returns sum of all positive integers from list [l]. 
   If their sum is bigger than max_int then function returns max_int*)
let safe_positive_list_addition l = 
  List.fold_left (fun acc x -> safe_positive_addition acc x) 0 l   


(* [in_interval (minv,maxv)] returns quantity of numbers in interval [(minv,maxv)]
   if there are more numbers in interval than max_int, function returns max_int*)
let in_interval (minv,maxv) =
  if(minv <= 0 && maxv >= 0) then
  safe_positive_addition maxv (1-minv)
  else maxv -minv + 1

(* [height t] returns height of tree [t]*)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* [count_elements t] returns quantity of numbers in all intervals in set [t] *)
let count_elements = function
  | Node (_,_,_,_,n) -> n
  | Empty -> 0

(* [make l (minv,maxv) r] returns set t = ([l], [(minv,maxv)], [r], h, n) where h is 
   calculated height of tree t, and n is calculated quantity of numbers
    in all intervals in set t *)
let make l (minv, maxv) r = 
  Node (l, (minv, maxv), r, max (height l) (height r) + 1, 
  safe_positive_list_addition [(count_elements l);(count_elements r);(in_interval (minv,maxv))]
  )


(* [bal l k r] returns balanced AVL tree made from 2 balanced 
   AVL trees [l] and [r] and interval [k] (value) where difference
   between heights of [l] and [r] is less than 4.*)
let bal l k r =   
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, 
  safe_positive_list_addition [(count_elements l);(count_elements r);(in_interval k)]
  )

(*[min_elt t] returns minimal element in set [t]*)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(*[ermove_min_elt t] returns set [t] without minimal element from set [t]*)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"


(*  [add_one x s] returns a set containing the same elements as [s],
    plus all elements of the interval x including [fst x] and [snd x].
    Assumes none of values from interval x exists in set [s]. *)
let rec add_one x = function 
  | Node (l, k, r, h, n) ->
      let c = cmp (fst x) k in
      if c = 0 then Node (l, k, r, h, n)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, in_interval x)



(*[find_interval x t] returns interval in set [t] containing x.
 If there's no such interval function returns (max_int, min_int)*)
let rec find_interval x = function
  |Node (l,k,r,h,n) ->
      let c = cmp x k in
      if c = 0 then k else 
      if c < 0 then find_interval x l 
               else find_interval x r  
  |Empty -> (max_int, min_int)

(* [join l v r] returns a balanced tree made by joining two balanced trees [l] and [r] and 
 interval [v] greater than every interval in [l] and lesser than every interval in [r] *)
let rec join l v r =                       
  match (l, r) with
  | (Empty, Empty) -> Node (Empty, v, Empty, 1, in_interval v)
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, ln), Node(rl, rv, rr, rh, rn)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else                                                                            
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(** [split x s] returns a triple [(l, present, r)], where
    [l] is the set of elements of [s] that are strictly lesser than [x];
    [r] is the set of elements of [s] that are strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x],
    or [true] if [s] contains an element equal to [x]. *)
let split x s =                                                                 
  let rec loop x = function
      Empty ->                                                                            
        (Empty, (1,-1) , Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp x v in
        if c = 0 then (l, v, r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, (join rl v r))                                                                            
        else
          let (lr, pres, rr) = loop x r in ((join l v lr), pres, rr)
  in
  let (setl, pres_int , setr) = loop x s in
    if (fst pres_int) > (snd pres_int) 
    then  (setl , false, setr)
    else  
    let minv = (fst pres_int)
    and maxv = (snd pres_int)
  in 
    let left_bool  = minv < x 
    and right_bool = maxv > x
  in  match (left_bool,right_bool) with 
      | (false,false) -> (setl, true, setr)
      | (true, false) -> ((add_one (minv, x-1) setl), true, setr)
      | (false, true) -> (setl, true, (add_one (x+1, maxv) setr))
      | (true,  true) -> ((add_one (minv, x-1) setl), true, (add_one (x+1, maxv) setr))

(** [add (minv, maxv) s] returns a set containing the same elements as [s],
    plus all elements of the interval [[minv,maxv]] including [minv] and [maxv].
    Assumes [minv <= maxv]. *)
let rec add(minv,maxv)  t =      
 let new_minv = min (minv)  (fst (find_interval (minv - 1) t))
 and new_maxv = max (maxv)  (snd (find_interval (maxv + 1) t))                                                                           
 in let (left, _, _)   = split new_minv t
    and (_, _, right)  = split new_maxv t
  in join left (new_minv , new_maxv) right 

(** [remove (minv, maxv) s] returns a set containing the same elements as [s],
    except for all those which are included between [minv] and [maxv].
    Assumes [minv <= maxv]. *)
let remove (minv,maxv) t =                                                           
    let (left, _, _)  = split minv t
    and (_, _, right) = split maxv t
  in if is_empty right then left else 
  join left (min_elt right) (remove_min_elt right)

(** [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
let mem x t =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop t

(** mem, but with different name*)
let exists = mem

(** [iter f s] applies [f] to all continuous intervals in the set [s].
    The intervals are passed to [f] in increasing order. *)
let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t

(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t

(** Return the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order. *)
let elements t = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t

(** [below n s] returns the number of elements of [s] that are less
    or equal [n]. If there are more than max_int such elements, 
    the result is max_int. *)
let below x t =
  let rec pom x t acc = 
  if acc = max_int then max_int else 
  match t with
  | Empty -> acc
  | Node(l,(minv,maxv),r,h,n) ->
    let c = cmp x (minv,maxv) in
    if c = 0 then safe_positive_list_addition [acc;(count_elements l);(in_interval (minv,(x)))]
  else if c < 0 then pom x l acc
  else pom x r (safe_positive_list_addition [acc;(count_elements l);(in_interval (minv,maxv))])
in pom x t 0


;;


(*
invariants of every function: 
-AVL tree condition
-Disjointness of every intervals in tree
-Minimal possible quantity of intervals 
 (there cannot be interval (a,b) and (b+1,c) 
 in one set, becouse they can be joined into (a,c)
 interval)
*)
