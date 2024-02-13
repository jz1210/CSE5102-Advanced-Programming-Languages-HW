(* Compute the cross product using only higher-order functions *)
(* List operator @ allowed *)

fun crossHigh a b = 
        let 
                fun crossWithX x = map(fn y => (x,y)) b
        in 
                foldr op@ [] (map crossWithX a)
        end;

fun printPair (x, y) = print("("^Int.toString(x)^","^Int.toString(y)^") ")

fun printList x = if null x then print("\n")
        else (printPair(hd(x)); printList (tl x));

printList (crossHigh [1, ~1, 2, ~2, 3, ~3] [ 1, 1, 2, 2, 3, 3]);
printList (crossHigh [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]);
printList (crossHigh [~5, ~4, ~3, ~2, ~1] [1, 2, 3, 4, 5]);
printList (crossHigh [5, 5, 4, 4, 3, 3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
