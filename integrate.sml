(* HW1 Question3 *)
fun integrate f a b n = 
    let 
        (* Calculate the step*)
        val h = (b - a) / real(n)
        (* Calculate the middle part of the trapezoidal rule because the first term is just f(a) and last term is f(b) *)
        fun midPart(i, value) = 
            if i = n then value
            else midPart(i + 1, value + 2.0 * f(a + real(i) * h))
    in 
        (* Apply the trapezoidal rule here *)
        h / 2.0 * (f(a) + midPart(1, 0.0) + f(b))
    end;

print(Real.toString(integrate Math.sin (~ Math.pi/2.0) 0.0 10000)^"\n");
print(Real.toString(integrate Math.sin (~ Math.pi) 0.0 10000)^"\n");
print(Real.toString(integrate Math.cos (~ Math.pi/2.0) 0.0 10000)^"\n");
