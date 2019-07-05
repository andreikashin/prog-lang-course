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

val test12 = first_match Unit [UnitP] = SOME []

