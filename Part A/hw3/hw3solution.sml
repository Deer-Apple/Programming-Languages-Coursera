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

fun only_capitals ss = 
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) ss

fun longest_string1 ss =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" ss

fun longest_string2 ss =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" ss

fun longest_string_helper comparator ss = 
    foldl (fn (x,y) => if comparator(String.size x, String.size y) then x else y) "" ss

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

fun rev_string ss = 
    String.implode(rev(String.explode ss))

fun first_answer f ss =
    case ss of
        [] => raise NoAnswer
      | x::xs' => case f x of
                      NONE => first_answer f xs'
                    | SOME y => y

fun all_answers f ss =
    let
        fun aux (xs, accu) =
            case xs of
                [] => SOME accu
              | x::xs' => case f x of
                            NONE => NONE
                          | SOME y => aux (xs', (accu @ y))
    in
        aux(ss, [])
    end

fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
        fun all_variable p = 
            case p of
	            Variable x        => [x]
	          | TupleP ps         => foldl (fn (p,i) => (all_variable p) @ i) [] ps
	          | ConstructorP(_,p) => all_variable p
	          | _                 => []
        (* return true if there is no duplicate *)
        fun check_dup ss =
            case ss of
                [] => true
              | s::ss' => if List.exists (fn x => x = s) ss'
                          then false
                          else check_dup ss'
    in
        check_dup (all_variable p)
    end

fun match (v, p) =
    case (v,p) of
        (_, Wildcard) => SOME []
      | (vv, Variable s) => SOME [(s, vv)]
      | (Unit, UnitP) => SOME []
      | (Const vv, ConstP pp) => if vv = pp then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs,ps))
                                 else NONE
      | (Constructor(s1,vv), ConstructorP(s2,pp)) => if s1 = s2 then match (vv,pp) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
        handle NoAnswer => NONE


(* for test *)
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val first_larger : t * t -> bool
end

structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0

end

(* NoNegativeCounter.first_larger(~1,~2); *)
(* NoNegativeCounter.first_larger(1,2); *)
    