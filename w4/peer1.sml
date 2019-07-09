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
fun only_capitals xs = (
    List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs
)

fun longest_string1 xs = List.foldl (fn (a, b) => (  if  ((String.size a) <= (String.size b)) then b else a ))  "" xs
fun longest_string2 xs = List.foldl (fn (a, b) => (  if  ((String.size a) >= (String.size b)) then a else b )) "" xs

fun longest_string_helper f =
    List.foldl (fn (str, acc) =>
		               if f (String.size str, String.size acc)
		               then str
		               else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => ( a > b ))
val longest_string4 = longest_string_helper (fn (a, b) => ( a >=b ))

val longest_capitalized = longest_string1 o only_capitals

val rev_string =  String.implode o List.rev o String.explode


fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
     |  x::t => case f(x) of
                    NONE => first_answer f t
                  | SOME v => v


fun all_answers f xs =
    let fun aux (f, xs, acc) =
        case xs of
            [] => SOME acc
         |  x::t => case f(x) of
                        NONE => NONE
                      | SOME v => aux(f, t, v @ acc)
    in
        aux (f, xs, [])
    end


val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (str, p) = g (fn _ => 0) (fn x =>
						                                    if String.isSubstring str x
						                                    then 1
						                                    else 0) p

(* Write a function check_pat that takes a pattern and returns true if and only if all the variables apperaing in the pattern are distinct from each other *)
fun check_pat p =
    let
        fun grab_string pat acc = case pat of
                                      Variable x => x :: acc
                                    | TupleP ps => List.foldl (fn (p, acc) => (grab_string p []) @ acc ) [] ps
                                    | ConstructorP (_, p) => grab_string p acc
                                    | _ =>  []
    in
        let
            val str_list = grab_string p []
            fun check_dup xs =
                case xs of
                    [] => true
                  | x::t => if List.exists (fn a:string => a = x) t
                            then false
                            else check_dup t
        in
            check_dup str_list
        end
    end


(* Write a function match that takes a valu * pattern and returns a (string * valu) list option,
   namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. *)
fun match (valu, pat) =
    case pat of
        Wildcard => SOME []
     |  UnitP => (case valu of Unit => SOME []
                            | _ => NONE)
     | Variable str => SOME [(str, valu)]
     | ConstP i => (case valu of Const j => if i = j then SOME [] else NONE
                            | _ => NONE)
     | TupleP plst => (case valu of
                          Tuple valu_list => if List.length plst = List.length valu_list
                                             then all_answers match (ListPair.zip (valu_list, plst))
                                             else NONE
                        | _  => NONE
                     )
     | ConstructorP (str, p) => (case valu of
                                     Constructor (vstr, vval) => if str = vstr then match (vval, p)
                                                               else NONE
                                  |  _ => NONE)


fun first_match v plst =
    SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE
