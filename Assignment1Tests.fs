module Assignment1Tests

// Test cases for Problem 1

// > nf 0;;
// val it: int = 0
// > nf 1;;
// val it: int = 1
// > nf 2;;
// val it: int = 2
// > nf 4;;
// val it: int = 6



// Test cases for Problem 2

// > truesAndLength [];;
// val it: int * int = (0, 0)
// > truesAndLength [true];;
// val it: int * int = (1, 1)
// > truesAndLength [true; false];;
// val it: int * int = (2, 1)
// > truesAndLength [true; false; false];;
// val it: int * int = (3, 1)
// > truesAndLength [true; false; false; true];;
// val it: int * int = (4, 2)
// > truesAndLength [true; false; false; true; true];;
// val it: int * int = (5, 3)

// > majority [];;
// val it: bool = true
// > majority [true];;
// val it: bool = true
// > majority [true; false];;
// val it: bool = true
// > majority [true; false; false];;
// val it: bool = false
// > majority [true; false; false; true];;
// val it: bool = true
// > majority [true; false; false; true; true];;
// val it: bool = true

// > majority2 (fun x -> x = "") [];;
// val it: bool = true
// > majority2 (fun x -> x = "") [""];;
// val it: bool = true
// > majority2 (fun x -> x = "") [""; "a"];;
// val it: bool = true
// > majority2 (fun x -> x = "") [""; "a"; "b"];;
// val it: bool = false
// > majority2 (fun x -> x = "") [""; "a"; "b"; ""];;
// val it: bool = true
// > majority2 (fun x -> x = "") [""; "a"; "b"; ""; ""];;
// val it: bool = true

// > majorityLarge [3;400;10];;
// val it: bool = false
// > majorityLarge [3;400;10;100];;
// val it: bool = true
// > majorityLarge [3;400;10;100;12];;
// val it: bool = false
// > majorityLarge [3;400;10;100;12;101;102];;
// val it: bool = true



// Test cases for Problem 3

// > isGood [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
// val it: bool = false
// > isGood [("p",4);("q",5);("q2",6);("r",6);("p",4)];;
// val it: bool = true
// > isGood [];;
// val it: bool = true
// > isGood [(0, 1); (0, 1)];;
// val it: bool = false
// > isGood [(0, 1); (1, 1); (0, 1)];;
// val it: bool = true
// > isGood [(0, 1); (1, 1); (0, 1); (0, 1)];;
// val it: bool = false

// > makeGoodInt [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", 11); ("r", 6); ("p", 4)]
// > makeGoodInt [("p",4);("q",5); ("q",6); ("q", -11);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", 0); ("r", 6); ("p", 4)]
// > makeGoodInt [("p",4);("q",5); ("q",6);("r",6);("q", -11);("p",4)];;
// val it: (string * int) list =
//   [("p", 4); ("q", 11); ("r", 6); ("q", -11); ("p", 4)]
// > makeGoodInt [("p",4);("q",6);("r",6);("q", -11);("p",4)];;
// val it: (string * int) list =
//   [("p", 4); ("q", 6); ("r", 6); ("q", -11); ("p", 4)]
// > makeGoodInt [("p",4);("p",10);("q",6);("r",6);("q", -11);("p",4); ("p",5)];;
// val it: (string * int) list =
//   [("p", 14); ("q", 6); ("r", 6); ("q", -11); ("p", 9)]

// > makeGoodWith (*) [("p",4);("q",5); ("q",6);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", 30); ("r", 6); ("p", 4)]
// > makeGoodWith (*) [("p",4);("q",5); ("q",6); ("q", -11);("r",6);("p",4)];;
// val it: (string * int) list = [("p", 4); ("q", -330); ("r", 6); ("p", 4)]
// > makeGoodWith (*) [("p",4);("q",5); ("q",6);("r",6);("q", -11);("p",4)];;
// val it: (string * int) list =
//   [("p", 4); ("q", 30); ("r", 6); ("q", -11); ("p", 4)]
// > makeGoodWith (+) [(0, "x"); (0, "y"); (1, "z")];;
// val it: (int * string) list = [(0, "xy"); (1, "z")]



// Test cases for Problem 4

// > shuffle2 [1..10];;
// val it: int list = [1; 3; 5; 7; 9; 10; 8; 6; 4; 2]
// > shuffle2 [1..11];;
// val it: int list = [1; 3; 5; 7; 9; 11; 10; 8; 6; 4; 2]
// > shuffle2 [-5..2];;
// val it: int list = [-5; -3; -1; 1; 2; 0; -2; -4]
// > shuffle2 [0];;
// val it: int list = [0]



// Test cases for Problem 5

// > foo2 [1..5];;
// val it: int option = Some 15
// > foo2 [1;2;3;4;3;2;1];;
// val it: int option = Some 16
// > foo2 [1;2;3;4;-3;2;1];;
// val it: int option = None
// > foo2 [-1];;
// val it: int option = None

// > foo2Default -5 [1..5];;
// val it: int = 15
// > foo2Default -5 [1;2;3;4;3;2;1];;
// val it: int = 16
// > foo2Default -5 [1;2;3;4;-3;2;1];;
// val it: int = -5
// > foo2Default -4 [-1];;
// val it: int = -4



// Test cases for Problem 7

let testTree =
  Branch (2,
    Branch (3,
      Branch (5,
        Leaf,
        Leaf),
      Leaf),
    Branch (7,
      Mul (11, Branch (13,
        Mul (17, Mul (19, Branch (23,
          Leaf,
          Leaf))),
        Branch (29, Leaf, Leaf))),
      Branch (31,
        Leaf,
        Branch (37,
          Leaf,
          Leaf))))


// > getValAt S testTree;;
// val it: int = 2
// > getValAt (L (L S)) testTree;;
// val it: int = 5
// > getValAt (R S) testTree;;
// val it: int = 7
// > getValAt (R (L S)) testTree;;
// val it: int = 143
// > getValAt (R (L (L S))) testTree;;
// val it: int = 81719

// > toTree Leaf;;
// val it: int tree = Lf
// > toTree (Branch (2, Leaf, Leaf));;
// val it: int tree = Br (2, Lf, Lf)
// > toTree (Mul (3, Branch (2, Leaf, Leaf)));;
// val it: int tree = Br (6, Lf, Lf)
// > toTree (Mul (5, Mul (3, Branch (2, Leaf, Leaf))));;
// val it: int tree = Br (30, Lf, Lf)
// > toTree (Mul (5, Branch (2, Mul (3, Leaf), Leaf)));;
// val it: int tree = Br (10, Lf, Lf)
// > toTree testTree;;
// val it: int tree =
//   Br
//     (2, Br (3, Br (5, Lf, Lf), Lf),
//      Br
//        (7, Br (143, Br (81719, Lf, Lf), Br (319, Lf, Lf)),
//         Br (31, Lf, Br (37, Lf, Lf))))



