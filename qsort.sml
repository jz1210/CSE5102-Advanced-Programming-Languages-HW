(* Implement qSort *)
fun qSort nil    = 


fun printList x = if null x then print("\n")
        else (print(Int.toString((hd x))^" "); printList (tl x));

printList (qSort [1, ~1, 2, ~2, 3, ~3]);
printList (qSort [5, 4, 3, 2, 1]);
printList (qSort [~5, ~4, ~3, ~2, ~1]);
printList (qSort [5, 5, 4, 4, 3, 3, 2, 2, 1, 1]);