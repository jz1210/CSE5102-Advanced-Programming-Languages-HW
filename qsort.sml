(* Helper function *)

fun partition (pivot,nil) = (nil, nil)  
        | partition (pivot,x::xs) = 
                let     
                        val (small, big) =  partition(pivot, xs)
                in      
                        if x < pivot
                        then (x::small, big)
                        else (small, x::big)
                end;

(* Implement qSort *)
fun qSort nil = nil
        |qSort l = 
                let 
                        val pivot = hd l
                        val rest = tl l 
                        val (small, big) = partition (pivot, rest)
                in 
                        qSort(small) @ pivot::qSort(big)
                end;


fun printList x = if null x then print("\n")
        else (print(Int.toString((hd x))^" "); printList (tl x));

printList (qSort [1, ~1, 2, ~2, 3, ~3]);
printList (qSort [5, 4, 3, 2, 1]);
printList (qSort [~5, ~4, ~3, ~2, ~1]);
printList (qSort [5, 5, 4, 4, 3, 3, 2, 2, 1, 1]);