(* 

Compiler Construction 
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 

Exercise Set 2. 

Topics : 
  a) Replacing tail-recursion with iteration. 
  b) CPS transform 
  c) Defunctionalisation 
*) 





(* Problem 1. 

   Again by hand, eliminate tail recursion from fold_left. 

   Does your source-to-source transformation 
   change the type of the function?  If so, 
   can you rewrite your code so that the type does not change? 

*) 


(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *) 
let rec fold_left f accu l =
  match l with
      [] -> accu
  | a::ls -> fold_left f (f accu a) ls

(* sum up a list *) 
let sum1 = fold_left (+) 0 [1;2;3;4;5;6;7;8;9;10]  



(* fold_left_iter: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *) 
let fold_left_iter f init l = 
  let not_done = ref true in
  let result = ref init in
  let xs = ref l in
  let _ = while !not_done do
    match !xs with 
       [] -> not_done := false
      | (y::ys) -> (result := (f y !result); xs := ys)
  done
  in !result;;

let () = print_string "\n"
let () = print_string (string_of_int (fold_left_iter (+) 0 [1;2;3;4;5;6;7;8;9;10]))
let () = print_string "\n"










(* Problem 2. 

   Apply (by hand) the CPS transformation to 
   the gcd code. 

   Explain your results. 

*) 

let rec gcd(m, n) = 
    if m = n 
    then m 
    else if m < n 
         then gcd(m, n - m)
         else gcd(m - n, n)

let gcd_test_1 = List.map gcd [(24, 638); (17, 289); (31, 1889)] 




(* since the original gcd is tail recursive, the continuation is simply the identity function *)
let rec gcd_cps(m, n, cnt) = 
  if m = n
  then cnt m 
    else if m < n 
         then gcd_cps(m, n - m, fun a -> a)
         else gcd_cps(m - n, n, fun a -> a)


let () = print_string "\n"
let () = print_string (string_of_int (   gcd_cps(24, 638, fun a -> a)  ))
let () = print_string "\n"










(* Problem 3. 

Environments are treated as function in interp_0.ml. 

Can you transform these definitions, starting 
with defunctionalisation, and arrive at a list-based
implementation of environments? 
*) 


(* update : ('a -> 'b) * ('a * 'b) -> 'a -> 'b *) 
let update(env, (x, v)) = fun y -> if x = y then v else env y

(* mupdate : ('a -> 'b) * ('a * 'b) list -> 'a -> 'b *) 
let rec mupdate(env, bl) = 
    match bl with 
    | [] -> env 
    | (x, v) :: rest -> mupdate(update(env, (x, v)), rest)

(* env_empty : string -> 'a *) 
let env_empty = fun y -> failwith (y ^ " is not defined!\n")

(* env_init : (string * 'a) list -> string -> 'a *) 
let env_init bl = mupdate(env_empty, bl) 



(* I don't understand how to properly defunctionalise these functions since they're fundamentally functional
    where all the examples so far have been simple arithmetic. This is my best attempt *)

(* intermediate working *)
(*type func_type = 
   UPDATE of string * int * func_type
  | ENVEMPTY
  | MUPDATE of func_type

let rec apply1 = function
  | (UPDATE(x, v, env), y) -> if x = y then v else apply1 (env, y)
  | (ENVEMPTY, y) -> failwith (y ^ " is not defined!\n")
  | (MUPDATE(env), bl:'a list) -> match bl with
                          | [] -> env
                          | ((x, v) :: rest) -> apply1(MUPDATE(UPDATE(x, v, env)), rest)

let rec eval1 = function
  | (ENV_INIT, bl:'a list) -> apply1(MUPDATE(ENVEMPTY), bl)*)


(* answer *)

type state =
   ENV_INIT
  | APPL_UPDATE of string * int * state
  | APPL_ENVEMPTY
  | APPL_MUPDATE of state

type acc =
    ACCLIST of (string * int) list
  | ACCTAG of string

type result =
    RESINT of int
  | RESFUN of state

let rec eval2 = function
  | (ENV_INIT, ACCLIST(bl)) -> eval2(APPL_MUPDATE(APPL_ENVEMPTY), ACCLIST(bl))
  | (APPL_UPDATE(x, v, env), ACCTAG(y)) -> if x = y then RESINT(v) else eval2(env, ACCTAG(y))
  | (APPL_ENVEMPTY, ACCTAG(y)) -> failwith (y ^ " is not defined!\n")
  | (APPL_MUPDATE(env), ACCLIST(bl)) -> match bl with
                                | [] -> RESFUN(env)
                                | (x, v) :: rest -> eval2(APPL_MUPDATE(APPL_UPDATE(x, v, env)), ACCLIST(rest))









(* Problem 4. 

   Below is the code for (uncurried) map, with an test using fib. 
   Can you apply the CPS transformation to map to produce map_cps? 
   Will this map_cps still work with fib?  If not, what to do? 

*) 

(* map : ('a -> 'b) * 'a list -> 'b list *) 
let rec map(f, l) = 
    match l with 
    | [] -> [] 
    | a :: rest -> (f a) :: (map(f, rest)) 

(* fib : int -> int *) 
let rec fib m =
    if m = 0 
    then 1 
    else if m = 1 
         then 1 
         else fib(m - 1) + fib (m - 2) 

let rec printListOfInts = function
  | [] -> print_string "\n"
  | x::xs -> let () = print_string "\n" in 
              let () = print_string (string_of_int (   x  )) in
              printListOfInts(xs);;




let () = print_string "\n"
let () = printListOfInts ( map(fib, [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) )
let () = print_string "\n"

let rec map_cps(f, l, cnt) =
  match l with 
    | [] -> cnt []
    | a :: rest -> map_cps(f, rest, fun xs -> cnt((f a) :: xs))


let () = print_string "\n"
let () = printListOfInts ( map_cps(fib, [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10], fun x -> x) )
let () = print_string "\n"

(* this map works with fib, what is it referring to that would cause it not to work? *)

