(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1solution.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test120 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                                (2011,4,28)], [2,3,4]) = 3
val test121 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                                (2011,4,28)], [2,3,4,3,2,3,4,3]) = 3
val test122 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                (2011,4,28)], [2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test123 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),
                (2011,4,28)], [2,3,4,3,2]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

(* val test130 = reasonable_date((2017,12,31)) = true
val test131 = reasonable_date((2017,1,1)) = true
val test132 = reasonable_date((2017,0,1)) = false
val test133 = reasonable_date((0,1,1)) = false
val test134 = reasonable_date((2017,2,29)) = false
val test135 = reasonable_date((2017,2,28)) = true
val test136 = reasonable_date((2016,2,29)) = true
val test137 = reasonable_date((0,2,29)) = false
val test138 = reasonable_date((1600,2,29)) = false *)