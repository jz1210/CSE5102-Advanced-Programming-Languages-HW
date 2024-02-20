datatype 'a Stream = Nil 
              | Cons of 'a * (unit -> 'a Stream);
exception Bad of string;
fun from seed next = Cons(seed, fn () => from (next seed) next);   

fun head (Nil)       = raise Bad("got nil in head")
   |head (Cons(a,b)) = a;

fun tail (Nil)       = raise Bad("got nil in tail")
   |tail (Cons(a,b)) = b();

fun take 0 stream = nil
	| take n Nil = raise Bad("got nil in take")
	| take n (Cons(h,t)) = h::(take (n-1) (t()));


(* write code below so that fs will hold the infinite factorial stream *)
val fs = 
(* n! = n · (n − 1)!, n times previous factorial *)
   let 
      fun factHelper (n, prevFac) = Cons(prevFac, fn() => factHelper (n+1.0 , n*prevFac))
   in
      factHelper(1.0, 1.0)
   end

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

printRealList (take 10 fs);

