(* HW1 Question 1.2: sumsq computes the sum of the squares of the first n naturals *)
fun sumsq n = if n = 0 then 0 else n * n + sumsq(n-1);

print(Int.toString(sumsq 1)^"\n");
print(Int.toString(sumsq 10)^"\n");
print(Int.toString(sumsq 100)^"\n");
print(Int.toString(sumsq 1000)^"\n");
