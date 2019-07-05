(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards(Wildcard) = 1
val test9a2 = count_wildcards(UnitP) = 0
val test9a3 = count_wildcards(ConstP 1) = 0
val test9a4 = count_wildcards(TupleP []) =  0
val test9a5 = count_wildcards(TupleP [Wildcard]) = 1
val test9a6 = count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]) = 1
val test9a7 = count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]) = 2
val test9a8 = count_wildcards(TupleP [TupleP [Wildcard, Wildcard]]) = 2
val test9a9 = count_wildcards(TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths(Wildcard) = 1
val test9b2 = count_wild_and_variable_lengths(UnitP) = 0
val test9b3 = count_wild_and_variable_lengths(ConstP 1) = 0
val test9b4 = count_wild_and_variable_lengths(TupleP []) =  0
val test9b5 = count_wild_and_variable_lengths(TupleP [Wildcard]) = 1
val test9b6 = count_wild_and_variable_lengths(TupleP [Wildcard, UnitP, Variable "abc"]) = 4
val test9b7 = count_wild_and_variable_lengths(
	TupleP [Wildcard, Wildcard, Variable "xy"]) = 4
val test9b8 = count_wild_and_variable_lengths(
	TupleP [TupleP [Wildcard, Wildcard, Variable "This is the sea"]]) = 17
val test9b9 = count_wild_and_variable_lengths(
	TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]) = 2

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var(
	"test",
	TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]) = 2
val test9c2 = count_some_var(
	"test",
	TupleP [TupleP [Wildcard, UnitP, Variable "te"]]) = 0
val test9c3 = count_some_var (
	"test",
	TupleP [TupleP [Wildcard, UnitP, Variable "test"]]) = 1
								  
val test10 = check_pat (Variable("x")) = true
val test10a1 = check_pat (
	TupleP [Wildcard,Variable "cat",
                Variable "pp",TupleP[Variable "tt"],
                Wildcard,ConstP 3,
                ConstructorP("cony",Variable "pp")]) = false
val test10a2 = check_pat (
	TupleP [Variable "cat",
                ConstructorP("cat",Wildcard)]) = true
val test10a3 = check_pat (
	TupleP [Wildcard,Variable "cat",
                Variable "pp",TupleP[Variable "tt"],
                Wildcard,ConstP 3,
                ConstructorP("tt",Variable "pq")]) = true
val test10a4 = check_pat (
	TupleP [
	    Wildcard, Variable "cat", Variable "pp",
	    TupleP [Variable "tt"], Wildcard, ConstP 3,
	    ConstructorP("cony",Variable "test")
	]
    ) = true
	    
val test11 = match (Const(1), UnitP) = NONE
val test11a1 = match(Unit, UnitP) = SOME []
val test11a2 = match(Unit, Variable "cat") = SOME [("cat", Unit)]
val test11a3 = match(Unit, ConstP 3) = NONE
val test11a4 = match(Tuple [], TupleP [Wildcard]) = NONE
val test11a5 = match(Tuple [Unit], TupleP []) = NONE
val test11a6 = match(Tuple [Unit], Variable "cat") = SOME [("cat", Tuple [Unit])]
val test11a7 = match(Tuple [Unit], TupleP [Variable "cat"]) = SOME [("cat", Unit)]
val test11a8 = match(Tuple [Unit, Const 8], TupleP [Variable "cat", ConstP 3]) = NONE
val test11a9 = match(
	Tuple [Unit, Const 8],
	TupleP [Variable "cat", Variable "dog"]
    ) = SOME [("cat", Unit),("dog", Const 8)]
	     
val test11b1 = match(
	Tuple [Unit, Tuple [Unit, Unit]],
	TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]
    ) = SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)]
	     
val test11b2 = match(Constructor ("mat", Unit), ConstructorP ("hat", Variable "cat")) = NONE
val test11b3 = match(
	Constructor ("dog", Unit),
	ConstructorP ("dog", Variable "cat")
    ) = SOME [("cat", Unit)]
	     
val test11b4 = match(
	Tuple[
	    Const 17, Unit, Const 7, Constructor ("zoe", Const 7),
	    Constructor ("zoe", (Constructor ("zoe", Const 7)))
	],
	TupleP[Wildcard,Wildcard]
    ) = NONE
	    
val test11b5 = match(Const 7, Wildcard ) = SOME []
val test11b6 = match(Unit, Wildcard ) = SOME []
val test11b7 = match(Tuple[Const 7], Wildcard ) = SOME []
val test11b8 = match(Constructor("cat", Const 7), Wildcard ) = SOME []
val test11b9 = match(Const 7, Variable "Zoe" ) = SOME [("Zoe", Const 7)]
val test11c1 = match(Unit, Variable "chopsticks" ) = SOME [("chopsticks", Unit)]
val test11c2 = match(Unit, UnitP ) = SOME []
val test11c3 = match(Const 7, UnitP ) = NONE
val test11c4 = match(Const 7, ConstP 7 ) = SOME []
val test11c5 = match(Const 7, ConstP 8 ) = NONE
val test11c6 = match(Constructor("Cat", Const 7), ConstructorP("Cat", Wildcard)) =  SOME[]
val test11c7 = match(Constructor("Dog", Const 7), ConstructorP("Cat", Wildcard)) =  NONE
val test11c8 = match(Constructor("Cat", Const 7), ConstructorP("Cat", UnitP)) =  NONE
val test11c9 = match(
	Constructor("Cat", Const 7),
	ConstructorP("Cat", Variable "dog")
    )  =  SOME [("dog", Const 7)]
	       
val test11d1 = match(Tuple[Const 7], TupleP[ConstP 7]) =  SOME []
val test11d2 = match(Tuple[Const 7], TupleP[ConstP 7,ConstP 7]) =  NONE
val test11d3 = match(
	Tuple[Const 7, Const 6, Unit, Const 8],
	TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]
    ) = SOME [("cat",Const 6)]
	     
val test11d4 = match(
	Tuple[Const 7, Const 6, Unit, Const 7],
	TupleP[Variable "a", Variable "ab", Variable "abc", Variable "abcd"]
    ) = SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)]


val test12 = first_match Unit [UnitP] = SOME []

