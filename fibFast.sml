(* HW1 Question 1.5: ML function fibFast that computes, in linear time, from an integer input n, the nth Fibonacci’s number *)

(* Implement the function fibFast *)
fun fibFast n = 
    (* Write a function that add from beginning*)
    let fun fast a b 0 = a 
        |   fast a b n = fast b (a + b) (n-1)
    in 
        fast 0 1 n 
    end;

print(Int.toString(fibFast 1)^"\n");
print(Int.toString(fibFast 10)^"\n");
print(Int.toString(fibFast 20)^"\n");
print(Int.toString(fibFast 30)^"\n");
