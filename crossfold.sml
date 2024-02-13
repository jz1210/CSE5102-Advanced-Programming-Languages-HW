(* Compute the cross product with a higher-order and no use of op@ *)		      
fun crossFold a b = foldr(fn (x, ls)=>foldr(fn (y, ls')=>((x,y)::ls')) ls b) [] a;

fun printPair (x, y) = print("("^Int.toString(x)^","^Int.toString(y)^") ")

fun printList x = if null x then print("\n")
        else (printPair(hd(x)); printList (tl x));

printList (crossFold [1, ~1, 2, ~2, 3, ~3] [ 1, 1, 2, 2, 3, 3]);
printList (crossFold [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]);
printList (crossFold [~5, ~4, ~3, ~2, ~1] [1, 2, 3, 4, 5]);
printList (crossFold [5, 5, 4, 4, 3, 3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
