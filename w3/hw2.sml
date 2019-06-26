(* Dan Grossman, Coursera PL, HW2 Provided Code *)
use "../w2/hw1.sml";
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

(* Helpers *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2
	     
fun all_except_item (item, all) =
    case all of
	[] => []
      | first::rest => if same_string(item, first)
		       then rest
		       else first::all_except_item(item, rest)

fun all_except_item_tail (all, item, result) =
    let fun append (ys, zs) =
		      case ys of
			  [] => zs
			| y::ys' => y::append(ys',zs)
		      
    in case all of
	   [] => result
	 | x::xs => if same_string(item, x)
		    then all_except_item_tail(xs,item,result)
		    else all_except_item_tail(xs,item,append(result,[x]))
    end

fun contains (item, items) =
    case items of
	[] => false
      | x::xs => item = x orelse contains(item,xs)
						  
fun append (xs,ys) =
    case xs of
        x::xs' => x :: append(xs',ys)
      | [] => ys
						  
(* put your solutions for problem 1 here *)
fun is_older (date1:int*int*int, date2:int*int*int) =
    #1 date1 < #1 date2 orelse 
    (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
    (#1 date1 = #1 date2 andalso 
     #2 date1 = #2 date2 andalso 
     #3 date1 < #3 date2)

fun number_in_month (dates:(int*int*int) list, month:int) =
    if null dates
    then 0 
    else number_in_month(tl dates, month) + 
	 (if (#2 (hd dates) = month)
	 then 1 
	 else 0)

fun number_in_months (dates:(int*int*int) list, months:int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + 
	 number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month (dates:(int*int*int) list, month:int) =
    if null dates
    then []
    else 
	if (#2 (hd dates) = month)
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months (dates:(int*int*int) list, months:int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@
	 dates_in_months(dates, tl months)

(* 6 *)
fun get_nth (values:string list, n:int) =
    if n = 1
    then hd values
    else get_nth (tl values, n-1)

(* 7 *)
fun date_to_string (date:int*int*int)=
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date)^" "^
	Int.toString(#3 date)^", "^
	Int.toString(#1 date)
    end

(* 8 *)
fun number_before_reaching_sum (sum:int, numbers:int list) =
    let 
	val new_sum = sum - hd numbers
    in
	if (new_sum <= 0)
	then 0
	else 1+number_before_reaching_sum(new_sum, tl numbers)
    end

(* 9 *)
fun what_month (day:int) =
    let val month_days =
	    [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, month_days)+1
    end

(* 10 *)
fun month_range (date1:int, date2:int) =
    if date2 < date1
    then []
    else what_month(date1) :: month_range(date1+1, date2)

(* 11 *)
fun oldest (dates:(int*int*int) list) =
    if null dates
    then NONE
    else 
	let fun max (xs:(int*int*int) list) =
		if null (tl xs)
		then hd xs
		else 
		    let val tl_ans = max(tl xs)
		    in
			if is_older(hd xs, tl_ans)
			then hd xs
			else tl_ans
		    end
	in
	    SOME (max(dates))
	end
		  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


	      
(* put your solutions for problem 2 here *)
fun all_except_option (item, items) =
    SOME (all_except_item(item, items))

fun get_substitutions1 (items_lists, item) =
    case items_lists of
	first_list::other_lists =>
	if contains(item, first_list)
	then append(all_except_item(item, first_list), get_substitutions1(other_lists,item))
	else get_substitutions1(other_lists,item)
      | _ => []
		  
fun get_substitutions2 (items_lists, item) =
    case items_lists of
	x::xs => if contains(item, x)
		 then append(all_except_item_tail(x,item, []), get_substitutions2(xs, item))
		 else get_substitutions2(xs, item)
      | _ => []

fun similar_names (name_lists, {first=x, middle=y, last=z}) =
    let fun combine (names, name) =
	    case names of
		[] => []
	      | x'::xs =>
		{first=x', middle=y, last=z}::combine(xs, name);
	val firstname = x
	val fullname = {first=x, middle=y, last=z}
    in
	fullname::combine(get_substitutions1(name_lists, firstname), firstname)
    end
	
fun card_color (suit, rank) =
    case suit of
	Clubs => Black
      | Spades => Black
      | Hearts => Red
      | Diamonds => Red

fun card_value (suit, rank) =
    case rank of
	Num x => x
      | Ace => 11
      | _ => 10
		 

				   
fun remove_card (cs, c, e) =
    let
	fun all_except (item, items) =
	    case items of
		[] => []
	      | x::xs => if item = x
			 then xs
			 else x::all_except(item, xs)

	fun has (item, items) =
	    case items of
		[] => false
	      | x::xs => x = item orelse has(item, xs)
    in
	if has(c, cs)
	then all_except(c, cs)
	else raise e
    end

fun all_same_color (cards) =
    case cards of
	[] => true
      | x::[] => true
      | x::x'::[] => card_color(x) = card_color(x')
      | x::x'::xs => card_color(x) = card_color(x') andalso all_same_color(x'::xs)

fun sum_cards (cards) =
    let
	fun f (cs, acc) =
	    case cs of
		[] => acc
	      | x::xs => f(xs, acc + card_value(x))

    in
	f(cards, 0)
    end

fun score (cards, goal) =
    let
	val sum = sum_cards(cards)
	val diff = sum - goal
	val pr_sc = if diff > 0 then 3 * diff else diff * ~1
    in
	if all_same_color(cards) then pr_sc div 2 else pr_sc
    end
	
fun officiate (cards, moves, goal) =
    let fun h (c::cs', [], held) = score(held, goal)
	  | h (cs, (Discard c)::ms, held) = h(cs, ms, remove_card(held, c, IllegalMove))
	  | h (c::cs', (Draw)::[], held) = score(c::held, goal)
	  | h ([],(Draw)::_,held) = score(held, goal)
	  | h (c::cs', (Draw)::ms, held) =	    
	    let	val sc = score(c::held, goal)
	    in	      			      
		if goal < sc
		then sc
		else h(cs',ms,c::held)
	    end
    in
	h(cards, moves, [])
    end
