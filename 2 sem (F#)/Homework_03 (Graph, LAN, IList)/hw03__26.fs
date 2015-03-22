open System

let rng = System.Random()

let GetInfProb OS =
  match OS with 
  | "Windows XP" -> 0.6
  | "Windows 8" -> 0.2
  | "Linux" -> 0.15
  | "Mac OS" -> 0.1
  | _ -> failwith "Unknown OS"

type Computer (num, OS) =
  class 
    let mutable inf = false

    member this.Number = num
    member this.OS = OS
    member this.IsInfected = inf

    member this.Infect prob =
      if prob <= GetInfProb OS then inf <- true

    member this.Info () =
      if inf then printfn "%A is infected" num
      else printfn "%A is not infected" num
  end

type LAN (List: string list, tag: bool array, list: (int list) array) =
  class
    let mutable m = 0
    let n = List.Length
    let c = [|for i in [0 .. n - 1] -> new Computer(i, List.[i])|]

    do
     for i = 0 to n - 1 do 
      if tag.[i] then c.[i].Infect(0.0)

    member this.InfectedNumber () =
      let mutable ans = 0
      for i in c do
        if i.IsInfected then ans <- ans + 1
      ans

    member this.Start() =
      m <- m + 1
      let inf = Array.filter (fun i -> c.[i].IsInfected) [|0 .. n - 1|]
      for i in inf do
        for j in list.[i] do
          let rnd = System.Random().NextDouble()
          c.[j].Infect rnd
    end

    member this.PrintStatus () =
      printf "Move: %A\nStatus:\n" m
      for i in c do i.Info()
      printfn "\n"
      

[<EntryPoint>]
let main argv = 
    let List = ["Windows XP"; "Windows XP"; "Mac OS"; "Linux"; "Windows 8"; 
      "Mac OS"; "Windows 8"]
    let tags = [| false; true; false; false; false; false; false; false; false |]
    let arrList = [| [7]; [2]; [1; 5]; [7];  [0; 1];  [1];  [4]; [2; 3; 4]; [6; 8] |]
    let lan = new LAN (List, tags, arrList)

    printf "Task 26: Local Network"
    lan.PrintStatus()
    for i in 1 .. 10 do
        lan.Start()
        lan.PrintStatus()
    0 