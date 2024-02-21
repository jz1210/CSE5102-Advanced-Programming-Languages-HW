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


(* Q8: Taylor expansion for Cosine *)

(* Write your code below *)
fun foldl f e nil = e
	| foldl f e (a::b) = foldl f (f a e) b

fun curry f = fn x => fn y=> f(x, y)
fun eval s x order = 
      let
        fun pow value = Cons(value:real, fn()=> pow (value*x))
        
        val p = pow 1.0
        
        fun terms Nil _ = Nil
            | terms (Cons(h:real,t)) p = Cons(h*head p, fn () => terms (t()) (tail p))
            
         val t = terms s p
      in
        foldl (curry op +) 0.0 (take order t)
      end

val even = from 0.0 (fn x => x + 2.0)

fun fac n Nil = Nil
   | fac n (Cons(h,t)) = Cons(n:real, fn() => fac (~1.0 * n * (h+1.0) * (h+2.0)) (t()))

fun frac Nil = Nil
   | frac (Cons(h:real,t)) = Cons(1.0 / h , fn() => frac (t()))

(* Also finish the next line *)
val coefs'' = frac (fac 1.0 even);

(* Read code on the next line carefully 
	to decdie what should be in coefs'' *)

fun cos x order = eval coefs'' (x*x) order
val v = cos (Math.pi) 10;

fun printRealList x = if null x then print("\n")
        else (print(Real.toString((hd x))^" "); printRealList (tl x));

printRealList (take 10 coefs'');
print(Real.toString(v)^"\n");

