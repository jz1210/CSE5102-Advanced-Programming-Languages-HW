(* HW1 Question 1.3:  function sumOdd which, given a natural n computes the sum of the first n odd naturals *)
fun sumOdd n = if n = 1 then 1 else 2 * n - 1 + sumOdd(n-1);

print(Int.toString(sumOdd 1)^"\n");
print(Int.toString(sumOdd 10)^"\n");
print(Int.toString(sumOdd 100)^"\n");
print(Int.toString(sumOdd 1000)^"\n");
