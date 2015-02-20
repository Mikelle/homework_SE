// simple arithmetic for Peano numbers
// Author: Mikhail Wall   

type Peano = Zero | S of Peano

let suc (p : Peano) = S p

let rec toInt a = 
  match a with
  | Zero -> 0
  | S a -> toInt a + 1

let rec plus a b = 
  match a with 
  | Zero -> b
  | S a -> S (plus a b)

let rec minus a b = 
  match a, b with
  | Zero, Zero -> Zero
  | S a, S b -> minus a b

let rec mult a b =
  match a with 
  | Zero -> Zero
  | S a -> plus b (mult a b)

let rec deg a b =
  match a, b  with
  | Zero, _ -> Zero
  | a, Zero -> S Zero 
  | S a, S b -> mult (S a) (deg (S a) b)

[<EntryPoint>]

let main args = 

  deg (S (S (S Zero))) (S (S Zero)) |> toInt |> printfn "%A"
  0

