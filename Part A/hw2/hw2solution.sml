(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, strs) = 
    let 
        fun remove_from_list (str, strs, accu) = 
            case strs of
                [] => accu
              | x::xs' => if same_string(x, str)
                          then remove_from_list(str, xs', accu)
                          else remove_from_list(str, xs', accu @ [x])
        val ret_list = remove_from_list(str, strs, [])
    in
        if ret_list = strs
        then NONE
        else SOME ret_list
    end

fun get_substitutions1 (substitutions, s) = 
    case substitutions of
        [] => []
      | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs', s)
                    | SOME result => result @ get_substitutions1(xs', s)


fun get_substitutions2 (substitutions, s) =  
    let 
        fun aux (substitutions, s, ret) = 
            case substitutions of
                [] => ret
              | x::xs' => case all_except_option(s, x) of
                              NONE => aux(xs', s, ret)
                            | SOME result => aux(xs', s, ret @ result)
    in
        aux(substitutions, s, [])
    end

fun similar_names (substitutions, name) = 
    let 
        val {first=fir, middle=mid, last=las} = name
        val sub_list = get_substitutions2(substitutions, fir)
        fun produce_similar_names (sub_list, accu) = 
            case sub_list of
                [] => accu
              | x::xs' => produce_similar_names(xs', accu @ [{first=x, middle=mid, last=las}])

    in
        produce_similar_names(sub_list, [name])
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
fun card_color (suit, _) = 
    case suit of
        Clubs => Black
      | Spades => Black
      | _ => Red

fun card_value (_, rank) = 
    case rank of
        Ace => 11
      | Num n => n
      | _ => 10

fun same_card(c1 : card, c2 : card) =
    c1 = c2

fun remove_card (cs, c, e) = 
    let
        fun aux (cs, accu) =
            case cs of
                [] => raise e
              | x::xs' => if same_card(x, c)
                          then accu @ xs'
                          else aux(xs', accu @ [x])
    in
        aux(cs, [])
    end

fun all_same_color (cs) = 
    case cs of 
        [] => true
      | _::[] => true
      | x::y::xs' => card_color(x) = card_color(y) andalso all_same_color(y::xs')

fun sum_cards (cs) = 
    let 
        fun aux (cs, accu) =
            case cs of 
                [] => accu
              | x::xs' => aux(xs', accu + card_value(x))
    in
        aux(cs, 0)
    end

fun score (cs, goal) =
    let
        val sum = sum_cards(cs)
        val pre_sorce = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color(cs)
        then pre_sorce div 2
        else pre_sorce
    end

fun officiate (cs (* card list *), ms (* move list *), goal) = 
    let
        fun aux (cs, ms, hs (* hold card *)) = 
            case ms of
                [] => score(hs, goal)
              | Discard c :: ms' => aux(cs, ms', remove_card(hs, c, IllegalMove))
              | Draw :: ms' => case cs of
                            [] => score(hs, goal)
                          | x::xs' => if sum_cards(x::hs) > goal
                                      then score(x::hs, goal)
                                      else aux(xs', ms', x::hs)
    in
        aux(cs, ms, [])
    end