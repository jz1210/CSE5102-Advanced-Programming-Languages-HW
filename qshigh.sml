(* Implement qsHigh using foldr *)
fun qsHigh nil    = nil
        |  qsHigh (x::xs) = 
                let 
                        val (small, big) = foldr (fn (pivot, (l, r)) => if pivot < x then (pivot::l, r) else (l, pivot::r)) ([], []) xs
                in 
                        qsHigh small @ (x :: qsHigh big)
                end;
                


fun printList x = if null x then print("\n")
        else (print(Int.toString((hd x))^" "); printList (tl x));

printList (qsHigh [1, ~1, 2, ~2, 3, ~3]);
printList (qsHigh [5, 4, 3, 2, 1]);
printList (qsHigh [~5, ~4, ~3, ~2, ~1]);
printList (qsHigh [5, 5, 4, 4, 3, 3, 2, 2, 1, 1]);
