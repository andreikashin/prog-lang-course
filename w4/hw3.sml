(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* Write a function only_capitals that takes a string list 
and returns a string list that has only
the strings in the argument that start with an uppercase letter. 
Assume all strings have at least 1 character. 
Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals (items) =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) items

fun longest_string1 (items) =
    List.foldl
	(fn (item, init) => if String.size(item) > String.size(init)
			    then item
			    else init
	)
	""
	items

fun longest_string2 (items) =
    List.foldr
	(fn (item, init) => if String.size(item) > String.size(init)
			    then item
			    else init
	)
	""
	items

fun longest_string_helper f items =
    List.foldl
	(fn (a, b) => if f (String.size(a), String.size(b))
		      then a
		      else b
	)
	""
	items

val longest_string3 =
    let val f = fn (a,b)=> a > b
    in
	longest_string_helper f
    end
	
val longest_string4 = longest_string_helper (fn (a, b) => a >= b )

val longest_capitalized = longest_string1 o only_capitals
						
val rev_string = String.implode o List.rev o String.explode

fun first_answer f items =
    case items of
	[] => raise NoAnswer
      | x::xs => case f(x) of
		     NONE => first_answer f xs
		   | SOME v => v 
    
fun all_answers f the_list =
    let
	fun h f acc items =
	    case items of
	    [] => SOME acc
	  | x::xs => case f(x) of
			 SOME l => h f (acc @ l) xs
		       | NONE => NONE
    in
	h f [] the_list
    end
	
fun count_wildcards i =
    g (fn x => 1) (fn y => 0) i

fun count_wild_and_variable_lengths i =
    g (fn x => 1) String.size i

fun count_some_var (var, i) =
    let fun h value =
	     if value = var
	     then 1
	     else 0
    in
	g (fn x => 0) h i
    end

fun check_pat pat =
    let
	fun h1 p acc =
	    case p of
		Variable x => [x] @ acc
	      | TupleP ps => List.foldl (fn (p,i) => (h1 p i)) acc ps
	      | ConstructorP(_,p) => h1 p acc 
	      | _ => acc

	fun h2 items =
	    case items of
		[] => false
	      | x::xs => not (List.exists (fn i => i = x) xs)

	
    in
	h2 (h1 pat [])
    end
		
(*
fun g2 f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
	
fun h3 p acc =
    case p of
	Variable x => [x] @ acc
      | TupleP ps => List.foldl (fn (p,i) => (h3 p i)) acc ps
      | ConstructorP(_,p) => h3 p acc 
      | _ => acc
		 
fun h4 items =
    case items of
	[] => false
      | x::xs => not (List.exists (fn i => i = x) xs)	

*)
fun match (v, p) = NONE
fun first_match v p = NONE
 
