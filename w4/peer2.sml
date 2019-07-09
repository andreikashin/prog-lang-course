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

fun only_capitals(ss) =
    List.filter(fn x => Char.isUpper(String.sub(x, 0))) ss

fun longest_string1(ss) =
    List.foldl(fn (x,y) => if String.size(x) > String.size(y) then x else y) "" ss

 fun longest_string2(ss) =
    List.foldl(fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" ss
	       
 fun longest_string_helper f ss =
     List.foldl(fn(x,y) => if f(String.size(x), String.size(y)) then x else y) "" ss

 val longest_string3 = longest_string_helper (fn (x, y) => x > y)
 val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

 val longest_capitalized = longest_string3 o only_capitals

						 
 val rev_string =  String.implode o List.rev o String.explode
     
 fun first_answer f xs =
     case xs of
	 [] => raise NoAnswer
       | x::xs' => case f x of
		       SOME x => x
		     | NONE => first_answer f xs'

					    
 fun all_answers f xs =
     let
	 fun aux(acc, xs) =
	     case xs of
		 [] => SOME acc
	       | x::xs'   => case f x of
				NONE => NONE
			     |  SOME y => aux(y@acc, xs') 
     in
	 aux([], xs)
     end


val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x=> 1) (fn x => String.size(x))			 
     
fun count_some_var(s, p) =
    g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
	fun all_vars (p, acc) =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, i) => all_vars(p, acc)@i) acc  ps
			  | _ => [] 
					
	fun repeats xs =
	    case xs of
		[] => true
		   | x::xs' => if List.exists (fn y => y = x) xs' then false else repeats xs' 
    in
	repeats (all_vars(p, []))
    end
	
	      
fun match(v, p) =
    case (v, p) of
	(_, Wildcard) => SOME[]
      | (_, Variable s) => SOME[(s, v)]
      | (Unit, UnitP) => SOME[]
      | (Const vv, ConstP pv) => if vv = pv then SOME[] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then match(v, p) else NONE 
      | _  => NONE 			


fun first_match v ps =
    SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
						 
    
