fun is_older (date1:int*int*int, date2:int*int*int) =
    #1 date1 < #1 date2 orelse 
    (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
    (#1 date1 = #1 date2 andalso 
     #2 date1 = #2 date2 andalso 
     #3 date1 < #3 date2)

fun number_in_month (dates:(int*int*int) list, month:int) =
    if null dates
    then 0 
    else number_in_month(tl dates, month) + 
	 (if (#2 (hd dates) = month)
	 then 1 
	 else 0)

fun number_in_months (dates:(int*int*int) list, months:int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + 
	 number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month (dates:(int*int*int) list, month:int) =
    if null dates
    then []
    else 
	if (#2 (hd dates) = month)
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months (dates:(int*int*int) list, months:int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@
	 dates_in_months(dates, tl months)

(* 6 *)
fun get_nth (values:string list, n:int) =
    if n = 1
    then hd values
    else get_nth (tl values, n-1)

(* 7 *)
fun date_to_string (date:int*int*int)=
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date)^" "^
	Int.toString(#3 date)^", "^
	Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum:int, numbers:int list) =
    let 
	val new_sum = sum - hd numbers
    in
	if (new_sum <= 0)
	then 0
	else 1+number_before_reaching_sum(new_sum, tl numbers)
    end

fun what_month (day:int) =
    let val month_days =
	    [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, month_days)+1
    end

fun month_range (date1:int, date2:int) =
    if date2 < date1
    then []
    else what_month(date1) :: month_range(date1+1, date2)

fun oldest (dates:(int*int*int) list) =
    if null dates
    then NONE
    else 
	let fun max (xs:(int*int*int) list) =
		if null (tl xs)
		then hd xs
		else 
		    let val tl_ans = max(tl xs)
		    in
			if is_older(hd xs, tl_ans)
			then hd xs
			else tl_ans
		    end
	in
	    SOME (max(dates))
	end

fun number_in_months_challenge () = 0


fun remove_duplicates (items: 'a list) = 0
    
