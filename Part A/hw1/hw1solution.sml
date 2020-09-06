fun is_older (age_1 : int*int*int, age_2 : int*int*int) = 
    if #1 age_1 <> #1 age_2
    then #1 age_1 < #1 age_2
    else if #2 age_1 <> #2 age_2
    then #2 age_1 < #2 age_2
    else #3 age_1 < #3 age_2

fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (ss : string list, index : int) =
    if index = 1
    then hd ss
    else get_nth(tl ss, index - 1)

fun date_to_string (date : int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, nums : int list) = 
    if sum <= hd nums
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month (days : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(days, months) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* fun oldest (dates : (int*int*int) list) = 
    if null dates
    then NONE
    else 
        let
            fun oldest_nonempty (ds : (int*int*int) list) = 
                if null (tl ds)
                then hd ds
                else
                    let val tl_ans = oldest_nonempty(tl ds)
                    in
                        if is_older(hd ds, tl_ans)
                        then hd ds
                        else tl_ans
                    end
        in
            SOME (oldest_nonempty dates)
        end *)
fun oldest(dates: (int*int*int) list) =
    if null(dates)
    then NONE
    else
        let
            fun oldest_nonempty(dates: (int*int*int) list) =
                if null(tl dates)
                then hd dates
                else
                    let
                        val candidate = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, candidate)
                        then hd dates
                        else candidate
                end
        in
            SOME (oldest_nonempty(dates))
        end

fun remove_duplicate (ss : int list) = 
    let 
        val sorted = ListMergeSort.sort (fn(x:int,y:int)=>(x>y)) ss
        fun remove_duplicate_sorted (ss : int list) = 
            if null ss
            then []
            else if null (tl ss)
            then ss
            else 
                if hd ss = hd (tl ss)
                then remove_duplicate_sorted(tl ss)
                else hd ss :: remove_duplicate_sorted(tl ss)
    in
        remove_duplicate_sorted (sorted)
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) = 
    number_in_months(dates, remove_duplicate(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicate(months))