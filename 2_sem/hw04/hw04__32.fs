open System
open NUnit.Framework

type IGraph<'A> = 
  interface
    abstract Nodes : 'A array
    abstract Size : int
    abstract HasEdge : 'A -> 'A -> bool
  end 

type ITGraph<'A, 'T> =
  interface
    inherit IGraph<'A>
    abstract Tags : 'T array
  end

let getInfProb OS = 
  match OS with
  | "Windows" -> 0.6
  | "Linux"   -> 0.3
  | "Android" -> 0.5
  | "Mac OS"    -> 0.2
  | _         -> failwith "Unknown OS"

type Computer (number, OS) =
  class
    let mutable infected = false

    member this.Number = number
    member this.OS = OS
    member this.IsInfected () = infected
      
    member this.Infect (prob) = 
      if prob <= getInfProb (OS) then infected <- true

    member this.Info () = 
      if infected then printfn "%d is infected" number 
      else printfn "%d is not infected" number
  end

type ComputerGraph (OSList : string list, tags : bool array, aList : (int list) array) =
  class
    let n = OSList.Length
    let comp = [|for i in [0 .. n - 1] -> new Computer(i, OSList.[i])|]

    interface ITGraph<Computer, bool> with
        member this.Nodes = comp
        member this.Size = n
        member this.HasEdge n1 n2 =
            List.exists (fun x -> x = n2.Number) aList.[n1.Number]
            
        member this.Tags = tags
  end

type LAN (graph : ITGraph<Computer, bool>) =
  class
    let mutable move = 0
    let n = graph.Size - 1

    do
      for i = 0 to n do
          if graph.Tags.[i] then graph.Nodes.[i].Infect(0.0)

    member this.InfectedNumber =
      let mutable answer = 0
      for i in graph.Nodes do
          if i.IsInfected() then answer <- answer + 1
      answer

    member this.Start c =
      move <- move + 1
      let inf = Array.filter (fun i -> graph.Nodes.[i].IsInfected()) [|0 .. n|]
      for i in inf do
        for j = 0 to n do
          if graph.HasEdge graph.Nodes.[i] graph.Nodes.[j] then
            let rnd = System.Random().NextDouble() + c
            graph.Nodes.[j].Infect(rnd)
      for i = 0 to n do
        if graph.Nodes.[i].IsInfected() then graph.Tags.[i] <- true
  end

let OSList = 
  [ "Linux"; "Windows"; "Android"; "Mac OS"; "Linux"; "Windows"; 
      "Linux"; "Windows"; "Mac OS"; "Android"]

[<TestCase ( 1.0, Result =  2)>] 
[<TestCase (-1.0, Result = 10)>] 
let ``Test 01: branched local network`` c =
  let tags = [| false; true; false; false; false; false; false; true; false; false |]
  let aList = 
      [| [1; 4]; [0; 2]; [1; 3; 4]; [2]; [0; 2; 5]; [4]; 
          [7; 8]; [6; 9]; [6; 9]; [7; 8] |]
    
  let graph = new ComputerGraph (OSList, tags, aList) :> ITGraph<Computer, bool>
  let network = new LAN (graph)
  let mutable n = 0

  if c = -1.0 then n <- 2 else n <- 10000
  for i = 0 to n do 
      network.Start(c)
  network.InfectedNumber

[<TestCase ( 1.0, Result =  1)>] 
[<TestCase (-1.0, Result = 10)>] 
let ``Test 02: linear local network`` c =
  let tags = [| true; false; false; false; false; false; false; false; false; false |]
  let aList = [| [1]; [2]; [3]; [4]; [5]; [6]; [7]; [8]; [9]; [] |]
    
  let graph = new ComputerGraph (OSList, tags, aList) :> ITGraph<Computer, bool>
  let network = new LAN (graph)
  let mutable n = 0

  if c = -1.0 then n <- 9 else n <- 10000
  for i = 0 to n do 
      network.Start(c) 
  network.InfectedNumber

[<EntryPoint>]
let main argv =
  0
 