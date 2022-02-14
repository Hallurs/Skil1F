// T-501-FMAL, Spring 2022, Assignment 1

(*
STUDENT NAMES HERE:
  Hallur Hermannsson Aspar
  Sigurður Sigurðsson
  ...
*)

module Assignment1



////////////////////////////////////////////////////////////////////////
// Problem 1                                                          //
////////////////////////////////////////////////////////////////////////

// nf : int -> int
let rec nf n = 
  if n < 1 then 0 
  elif n = 1 then 1
  else nf(n - 2) + n 

nf 0;;
nf 1;;
nf 2;;
nf 4;;

////////////////////////////////////////////////////////////////////////
// Problem 2                                                          //
////////////////////////////////////////////////////////////////////////

// truesAndLength : bool list -> int * int
let rec truesAndLength bs = 
  let LT x y = (fst x + fst y, snd x + snd y)
  match bs with
  | [] -> 0, 0
  | head::tail ->
    if head then LT (1,1) (truesAndLength tail) 
    else LT (1,0) (truesAndLength tail)

truesAndLength [];;
truesAndLength [true];;
truesAndLength [true; false];;
truesAndLength [true; false; false];;
truesAndLength [true; false; false; true];;
truesAndLength [true; false; false; true; true];;

// majority : bool list -> bool
let rec majority bs = 
  let tupla = truesAndLength bs
  if float(fst tupla)/2.0 <= float(snd tupla) then true
  else false

majority [];;
majority [true];;
majority [true; false];;
majority [true; false; false];;
majority [true; false; false; true];;
majority [true; false; false; true; true];;


// majority2 : ('a -> bool) -> 'a list -> bool
let majority2 p xs= 
  let newList = List.map(fun x -> p x) xs
  majority newList




majority2 (fun x -> x = "") [];;
majority2 (fun x -> x = "") [""];;
majority2 (fun x -> x = "") [""; "a"];;
majority2 (fun x -> x = "") [""; "a"; "b"];;
majority2 (fun x -> x = "") [""; "a"; "b"; ""];;
majority2 (fun x -> x = "") [""; "a"; "b"; ""; ""];;

// majorityLarge : int list -> bool
let majorityLarge xs = 
  majority2 (fun x -> if x >= 100 then true else false) xs


majorityLarge [3;400;10];;
majorityLarge [3;400;10;100];;
majorityLarge [3;400;10;100;12];;
majorityLarge [3;400;10;100;12;101;102];;


////////////////////////////////////////////////////////////////////////
// Problem 3                                                          //
////////////////////////////////////////////////////////////////////////

// isGood : ('a * 'b) list -> bool when 'a: equality
let rec isGood ps = 
  match ps with
  | x::(x'::_ as tail) -> if fst x = fst x' then false else (isGood tail)
  | [] | [_] -> true

isGood [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
isGood [("p",4);("q",5);("q2",6);("r",6);("p",4)];;
isGood [];;
isGood [(0, 1); (0, 1)];;
isGood [(0, 1); (1, 1); (0, 1)];;
isGood [(0, 1); (1, 1); (0, 1); (0, 1)];;

// makeGoodInt : ('a * int) list -> ('a * int) list when 'a: equality
let rec makeGoodInt ps = 
  match ps with
  | [] -> []
  | x::x'::ps when fst x = fst x' -> makeGoodInt((fst x, snd x + snd x')::ps) 
  | x::ps -> (fst x, snd x)::makeGoodInt ps
  

makeGoodInt [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
makeGoodInt [("p",4);("q",5); ("q",6); ("q", -11);("r",6);("p",4)];;
makeGoodInt [("p",4);("q",5); ("q",6);("r",6);("q", -11);("p",4)];;
makeGoodInt [("p",4);("q",6);("r",6);("q", -11);("p",4)];;
makeGoodInt [("p",4);("p",10);("q",6);("r",6);("q", -11);("p",4); ("p",5)];;

// makeGoodWith : ('b -> 'b -> 'b) -> ('a * 'b) list -> ('a * 'b) list
//                                                     when 'a: equality
let rec makeGoodWith f ps = 



makeGoodWith (*) [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", 30); ("r", 6); ("p", 4)]
makeGoodWith (*) [("p",4);("q",5); ("q",6); ("q", -11);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", -330); ("r", 6); ("p", 4)]
makeGoodWith (*) [("p",4);("q",5); ("q",6);("r",6);("q", -11);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", 30); ("r", 6); ("q", -11); ("p", 4)]
makeGoodWith (+) [(0, "x"); (0, "y"); (1, "z")];;
// val it: (int * string) list = [(0, "xy"); (1, "z")]



////////////////////////////////////////////////////////////////////////
// Problem 4                                                          //
////////////////////////////////////////////////////////////////////////

// shuffle : 'a list -> 'a list
let rec shuffle xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> x :: (shuffle xs @ [y])

(*
ANSWER 4(i) HERE:
  ...
*)


// shuffle2 : 'a list -> 'a list
let shuffle2 xs =
  // shuffleAcc : 'a list -> 'a list -> 'a list
  let rec shuffleAcc acc xs = failwith "Not implemented"
  shuffleAcc [] xs



////////////////////////////////////////////////////////////////////////
// Problem 5                                                          //
////////////////////////////////////////////////////////////////////////

exception FooException

// foo : int list -> int
let rec foo xs =
  match xs with
  | [] -> 0
  | x::xs -> if x < 0 then raise FooException else x + foo xs

// fooDefault : int -> int list -> int
let fooDefault d xs = try foo xs with FooException -> d

// foo2 : int list -> int option
let rec foo2 xs = failwith "Not implemented"

// foo2Default : int -> int list -> int
let foo2Default d xs = failwith "Not implemented"



////////////////////////////////////////////////////////////////////////
// Problem 6                                                          //
////////////////////////////////////////////////////////////////////////

type mtree =
  | Leaf                            // leaf
  | Branch of int * mtree * mtree   // branch (value, left, right)
  | Mul of int * mtree              // multiply values below by given int

type 'a tree =
  | Lf                              // leaf
  | Br of 'a * 'a tree * 'a tree    // branch (value, left, right)

type pos =
  | S                               // the root ("stop") position
  | L of pos                        // a position in the left subtree
  | R of pos                        // a position in the right subtree

// getValAt : pos -> mtree -> int
let rec getValAt p t = failwith "Not implemented"

// let rec getValAt p t =
//   match p, t with
//   | S,   t             -> t
//   | Leaf p, Leaf            -> failwith "Cannot continue"
//   | L p, Branch (_, t1, _) -> getValAt p t1
//   | R p, Leaf            -> failwith "Cannot continue"
//   | R p, Branch (_, _, t2) -> getValAt p t2

// toTree : mtree -> int tree
let toTree t = failwith "Not implemented"

