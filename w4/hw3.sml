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
 fn (items) =>
    let val f = fn (a,b)=> a>b
    in
	List.foldl
	    (fn (a, b) => if f (String.size(a), String.size(b))
			  then a
			  else b
	    )
	    ""
	    items
    end
	
val longest_string4 =
 fn (items) =>
    let val f = fn (a,b)=> a>b
    in
	List.foldr
	    (fn (a, b) => if f (String.size(a), String.size(b))
			  then a
			  else b
	    )
	    ""
	    items
    end
(*
val longest_capitalized =
 fn items =>
    let
	val f =
	 fn (a,b) => if String.size(a) > String.size(b)
		     then a
		     else b
	val g = fn (item) => Char.isUpper(String.sub(item,0))
    in
	List.foldl
	    (fn (a, b) => if g o f (a, b)		  
			  then a
			  else b
	    )
	    ""
	    items
    end
*)
	
fun rev_string items =
    let val chars = String.explode(items)
	fun rev letters =
	    case letters of
		[] => ""
	      | x::xs => rev(xs) ^ String.str(x)
				       
    in
	rev(chars)
    end
