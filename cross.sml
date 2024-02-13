(* Implement cross using list pattern matching and reursion*)
fun cross nil b = nil
        | cross b nil = nil
        | cross (x::xs) b  = 
                let     
                        fun crossWithY y = (x,y)
                        fun groupTogether nil ls = ls
                                |  groupTogether (y::ys) ls =   groupTogether ys (ls @ [crossWithY y])
                                in 
                                        groupTogether b [] @ cross xs b
                end;


fun printPair (x, y) = print("("^Int.toString(x)^","^Int.toString(y)^") ")

fun printList x = if null x then print("\n")
        else (printPair(hd(x)); printList (tl x));

printList (cross [1, ~1, 2, ~2, 3, ~3] [ 1, 1, 2, 2, 3, 3]);
printList (cross [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]);
printList (cross [~5, ~4, ~3, ~2, ~1] [1, 2, 3, 4, 5]);
printList (cross [5, 5, 4, 4, 3, 3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
