(* HW1 Question2 *)
(* Implement the function sinappx *)
fun sinappx n x = 
(* use function to add all the term together from first term to last *)
    let fun sigma(value, term, count) = 
            if count > 2.0 * real(n) then value
            else 
                let
                    (* Calculate nextTerm based on current term to prevent factorial function overflow *)
                    val nextTerm = (~1.0) * term * x * x /((count + 1.0) * (count + 2.0))
                in
                    sigma(value + nextTerm, nextTerm, count + 2.0)
                end
    in 
        if n = 0 then x else sigma(x, x, 1.0)
    end;

print(Real.toString(sinappx 1000 0.0) ^ "\n");
print(Real.toString(sinappx 1000 (Math.pi/2.0)) ^ "\n");
print(Real.toString(sinappx 1000 (~ Math.pi/2.0)) ^ "\n");
