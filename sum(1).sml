(* HW1 Question 1.1: function sum which, given an integer n computes the sum of the first n naturals *)
fun sum n = if n = 0 then 0 else n + sum (n - 1);

print(Int.toString(sum 1)^"\n");
print(Int.toString(sum 10)^"\n");
print(Int.toString(sum 100)^"\n");
print(Int.toString(sum 1000)^"\n");





