module app

open System.Threading

let maxInArr (arr : int []) l r : int =
  let mutable res = arr.[l]
  for i in l + 1 .. r do
    if res < arr.[i]
    then res <- arr.[i]
  res

let max threadNumber (arr:int[]) =
  let arraySize = arr.Length 
  let step = arraySize / threadNumber
  let res  = ref (maxInArr arr (arr.Length - step) (arr.Length - 1))
  let threadArray = Array.init threadNumber (fun i ->
    new Thread(ThreadStart(fun _ ->
      let threadRes = maxInArr arr (i * step) ((i + 1) * step - 1)
      lock res (fun _ -> if !res < threadRes then res := threadRes)
      ))
    ) 
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

let integrate (f : double -> double) l r h =
  let mutable res = 0.0
  for i in l .. h .. (r - h) do 
    res <- res + ((f i) + (f (i + h ))) * h * 0.5
  res

let calcIntegral (f : double -> double) l r step threadNumber = 
  let interval = (r - l) / double threadNumber
  let res = ref 0.0
  let threadArray = Array.init threadNumber (fun i ->
    new Thread(ThreadStart(fun _ ->
      let threadRes = 
        integrate f (l + double i * interval) (l + (double i + 1.0) * interval) step
      Monitor.Enter(res)
      res := !res + threadRes
      Monitor.Exit(res)
    )
  ))
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Task: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue

[<EntryPoint>]
let main argv = 
    //duration (sprintf "qs p %i" 2) (fun () -> max 4 [|for i in 0..10000000 -> i|]) |> ignore
    //duration (sprintf "%i" 4) (fun () -> calcIntegral (fun x -> id x ) -10000000.0 0.0 0.5 1) |> ignore
    0 



