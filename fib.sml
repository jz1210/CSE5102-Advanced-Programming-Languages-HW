(* HW1 Question 1.4: ML function fib that computes, from an integer input n, the nth Fibonacci number *)
fun fib n = if n = 0 then 0 
            else if n = 1 then 1 
            else fib(n-1) + fib(n-2);

print(Int.toString(fib 1)^"\n");
print(Int.toString(fib 10)^"\n");
