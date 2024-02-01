(* Implement the function variance *)
fun variance xs = 
    let 
        (* use length to get the total term and change it to real number *)
        val n = real(length(xs));
        (* Add all the number and number square in the list with fold function *)
        fun sumList ys = foldl(op +) 0.0 ys
        fun sumSqList ys = foldl(fn (x, acc) => x * x + acc) 0.0 ys
        (* Calculate the average of the squares and the the average *)
        val aveSq = sumSqList(xs) / n
        val ave = sumList(xs) / n
    in 
        (*  average of the squares minus the square of the average *)
        aveSq - ave * ave 
    end;

print(Real.toString(variance [1.0, 2.0, 3.0, 4.0, 5.0])^"\n");
print(Real.toString(variance [~1.0, ~2.0, ~3.0, ~4.0, ~5.0])^"\n");
print(Real.toString(variance [~1.0, 1.0, ~1.0, 1.0, ~1.0, 1.0])^"\n");
print(Real.toString(variance [~2.0, 2.0, ~2.0, 2.0, ~2.0, 2.0])^"\n");
