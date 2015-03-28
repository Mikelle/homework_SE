open System

type IGraph<'A> =
  interface
    abstract Size: int
    abstract Nodes: 'A array
    abstract HasEdge: 'A -> 'A -> bool
  end

type ITGraph<'A, 'T> =
  interface
    inherit IGraph<'A>
    abstract Tags: 'T array
  end

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
    member this.IsInfected () = inf

    member this.Infect prob =
      if prob <= GetInfProb OS then inf <- true

    member this.Info () =
      if inf then printfn "%A is infected" num
      else printfn "%A is not infected" num
  end

type GraphComputer (OSList: string list, tags: bool array, list: (int list) array) =
  class
    let n = OSList.Length
    let comp = [|for i in [0 .. n - 1] -> new Computer(i, OSList.[i])|]

    interface ITGraph<Computer, bool> with
      member this.Nodes = comp
      member this.Size = n
      member this.HasEdge n1 n2 =
        List.exists (fun x -> x = n2.Number) list.[n1.Number]
      member this.Tags = tags
  end


type LAN (graph: ITGraph<Computer, bool>) =
  class
    let mutable m = 0

    do 
      for i = 0 to graph.Size - 1 do
        if graph.Tags.[i] then graph.Nodes.[i].Infect(0.0)

    member this.InfectedNumber () =
      let mutable ans = 0
      for i in graph.Nodes do
        if i.IsInfected() then ans <- ans + 1
      ans

    member this.Start () =
      m <- m + 1
      let inf = Array.filter (fun i -> graph.Nodes.[i].IsInfected()) [|0 .. graph.Size - 1|]
      for i in inf do
        for j = 0 to graph.Size - 1 do
          if graph.HasEdge graph.Nodes.[i] graph.Nodes.[j] then
            let rnd = System.Random().NextDouble()
            graph.Nodes.[j].Infect(rnd)
      for i = 0 to graph.Size - 1 do
        if graph.Nodes.[i].IsInfected() then graph.Tags.[i] <- true
    end

    member this.PrintStatus () =
      printf "\n\nMove: %A\nStatus:\n" m
      let c = [|for i in graph.Nodes -> if i.IsInfected() then '!' else ' '|]
      printfn "
      (1. Windows 8)%c -- (2. Windows XP)%c -- (3. Mac OS   )%c
       |                  |
       |                  |
      (0. Linux    )%c -- (4. Linux     )%c -- (5. Windows 8)%c
            
            
      (6. Linux    )%c -- (7. Windows 8)%c
       |                  |
       |                  |
      (8. Mac OS   )%c -- (9. Windows XP)%c
      \n\n" c.[0] c.[1] c.[2] c.[3] c.[4] c.[5] c.[6] c.[7] c.[8] c.[9]
      for i in graph.Nodes do i.Info()
      printf "\nPress any key to continue . . . "
      match System.Console.ReadKey().Key with | _ -> ()


[<EntryPoint>]
let main argv = 
    
    let OSList = 
        [ "Linux"; "Windows 8"; "Windows XP"; "Mac OS"; "Linux"; "Windows 8"; 
            "Linux"; "Windows 8"; "Mac OS"; "Windows XP"]
    
    let labels = [| false; false; true; false; false; false; false; true; false; false |]
    let aList = 
        [| [1; 4;]; [0; 2;]; [1; 3; 4;]; [2;]; [0; 2; 5;]; [4;]; 
            [7; 8;]; [6; 9;]; [6; 9;]; [7; 8;]; |]
    
    let graph = new GraphComputer (OSList, labels, aList)
    let network = new LAN (graph)
    
    printf "Press any key to continue . . . "
    match System.Console.ReadKey().Key with | _ -> ()
    
    printf "\n\n\nTask 26: Local Network: "
    network.PrintStatus()
    while network.InfectedNumber() < OSList.Length do
        network.Start()
        network.PrintStatus()
    
    0