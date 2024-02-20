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

(* Implement weave here *)

fun weave Nil b =  b
   | weave a Nil = a
   | weave (Cons(a,fa)) (Cons(b,fb)) = Cons(a,fn()=>Cons(b,fn()=>weave (fa()) (fb())))


val odd = from 1.0 (fn x => x + 2.0);
val even = from 2.0 (fn x => x + 2.0);

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

printRealList (take 10 (weave odd even));
