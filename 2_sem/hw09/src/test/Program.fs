open app
open NUnit.Framework

[<TestCase(1, [|1; 2; 3; 4|], Result = 4)>]
[<TestCase(2, [|1; 2; 3; 4|], Result = 4)>]
[<TestCase(3, [|4; 3; 2; 1|], Result = 4)>]
[<TestCase(4, [|7; 2; 8; 6; 11|], Result = 11)>]
let TestMax n arr =
  max n arr

[<TestCase(0.0, 3.0, 0.5, 1, Result = 7.5)>]
[<TestCase(-3.0, 0.0, 1, 2, Result = -1.5)>]
let TestInt l r s threadNumber =
  calcIntegral (fun x -> x + 1.0) l r s threadNumber

[<TestCase(0.0, 3.0, 1, 1, Result = 2.25)>]
[<TestCase(-4.0, 2.0, 0.5, 2, Result = -3.0)>]
let TestIntDiv l r s threadNumber =
  calcIntegral (fun x -> x / 2.0) l r s threadNumber

(*
  Elapsed Time:
    Time for integral calculate:
      Interval : 10 ^ 8
      13386 : 1 thread
      12552 : 2 threads
      9367  : 4 threads
      9187  : 8 thread  
      9035  : 16 threads
      9140  : 32 threads

      Interval : 10 ^ 7
      2082  : 1 thread
      1309  : 2 threads
      964   : 4 threads
      988   : 8 threads

      Interval : 10 ^ 6
      205   : 1 thhread
      157   : 2 threads
      99    : 4 threads
      136   : 8 threads

    Time for finding max in array:
      Interval : 10 ^ 8
      19504 : 1 thread
      16437 : 2 threads
      15484 : 4 threads
      15314 : 8 threads
      15270 : 16 threads
      15435 : 32 threads

      Interval : 10 ^ 7
      1634  : 1 thread
      1585  : 2 threads
      1578  : 4 threads
      1561  : 8 threads
      1567  : 16 threads
      1585  : 32 threads
    *)

[<EntryPoint>]
let main argv = 
  0