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
    let fun h (c::cs', [], held) =
	    (print("no more mvs");score(held, goal))
	  | h (cs, (Discard c)::ms, held) =
	    (print("disc\n"); h(cs, ms, remove_card(held, c, IllegalMove)))
	  | h (c::cs', (Draw)::[], held) =
	    (print("last draw\n");score(c::held, goal))
	  | h ([],(Draw)::ms,held) = (print("empty cs\n"); score(held, goal))
	  | h (c::cs', (Draw)::ms, held) =
	    (print("drawing");
	     print(Int.toString(card_value(c)));
	    let	val sc = score(c::held, goal)
	    in	      			      
		if goal > sc
		then (print("end\n");sc)
		else (print("cont\n");h(cs',ms,c::held))
	    end)
		
		    
    in
	h(cards, moves, [])
    end
