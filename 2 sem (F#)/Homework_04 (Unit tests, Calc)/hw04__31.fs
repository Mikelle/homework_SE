open NUnit.Framework

type IGraph<'A> =
  interface
    abstract Size: int
    abstract Nodes: 'A array
    abstract HasEdge: 'A -> 'A -> bool
  end

type MGraph<'A when 'A: equality> (a: 'A array, matrix : (bool array) array) =
  class 
    interface IGraph<'A> with
      member this.Nodes = a
      member this.Size = a.Length
      member this.HasEdge n1 n2 =
        let i1 = Array.findIndex (fun x -> x = n1) a
        let i2 = Array.findIndex (fun x -> x = n2) a
        matrix.[i1].[i2]
  end

type LGraph<'A when 'A: equality> (arr: 'A array, arrayOfLists: ('A list) array) =
  class 
    interface IGraph<'A> with
      member this.Nodes = arr
      member this.Size = arr.Length
      member this.HasEdge n1 n2 =
        let i = Array.findIndex (fun x -> x = n1) arr
        List.exists (fun x -> x = n2) arrayOfLists.[i]
   end

let rec dfs (graph: IGraph<'A>) (visited: bool array) node f =
  let mutable res = []
  let index = Array.findIndex (fun x -> x = node) graph.Nodes
  visited.[index] <- true
  for i in 0..graph.Size - 1 do
    if not visited.[i]
      && (f = 0 && graph.HasEdge node graph.Nodes.[i]
        || f  = 1 && graph.HasEdge graph.Nodes.[i] node)
    then 
      visited.[i] <- true
      res <- List.append res [graph.Nodes.[i]]
      res <- List.append res (dfs graph visited graph.Nodes.[i] f)
  res

let availableFrom (graph: IGraph<int>) node =
  if (node < 0) || (node > graph.Size - 1) then printfn "Index out of range"; [-1]
  else
    let visited = [|for i in 0..graph.Size - 1 -> false|]
    dfs graph visited node 0

let availableIn (graph: IGraph<int>) node =
  if (node < 0) || (node > graph.Size - 1) then printfn "Index out of range"; [-1]
  else
    let visited = [|for i in 0..graph.Size - 1 -> false|]
    dfs graph visited node 1

type ITGraph<'A, 'T> =
  interface
    inherit IGraph<'A>
    abstract taggs: 'T array
    abstract getTag: int -> 'T
  end

let example = [|5; 6; 3; 9; 2; 1; 4;|] 
let ex1 = [||]
let fls = false
let matrix = 
    [| 
        [| fls;  fls;  fls;  fls;  fls;  fls;  fls;|]; 
        [| fls;  fls; true;  fls;  fls;  fls; true;|]; 
        [| fls;  fls;  fls; true;  fls;  fls;  fls;|]; 
        [| fls;  fls; true;  fls;  fls;  fls;  fls;|]; 
        [| fls;  fls;  fls;  fls;  fls; true;  fls;|]; 
        [|true;  fls;  fls;  fls;  fls;  fls;  fls;|]; 
        [|true;  fls;  fls; true; true;  fls;  fls;|];
    |]
let arrayOfLists = [| []; [3; 4]; [9]; [3]; [1]; [5]; [2; 5; 9] |]

let mgraph = new MGraph<int> (example, matrix)
let emgraph = new MGraph<int> (ex1, matrix)

let elgraph = new LGraph<int> (ex1, arrayOfLists)
let lgraph = new LGraph<int> (example, arrayOfLists)

[<TestCase(1, Result = [|5|])>]  
[<TestCase(2, Result =[|1; 5|])>]
[<TestCase(3, Result = [|9|])>]
[<TestCase(4, Result = [|5; 9; 3; 2; 1|])>]
[<TestCase(5, Result = [||])>]
[<TestCase(100, Result = [|-1|], TestName = "Index out of range")>]
let ``Test 01: Nodes which available from nodes (using MGraph)``ex  =
  List.toArray(availableFrom mgraph ex)

[<TestCase(1, Result = [|5|])>]  
[<TestCase(2, Result =[|1; 5|])>]
[<TestCase(3, Result = [|9|])>]
[<TestCase(4, Result = [|5; 9; 3; 2; 1|])>]
[<TestCase(5, Result = [||])>]
[<TestCase(100, Result = [|-1|], TestName = "Index out of range")>]
let ``Test 02: Nodes which available from nodes (using LGraph)``ex =
  List.toArray(availableFrom lgraph ex)

[<TestCase(1, Result = [|2; 4; 6|])>]
[<TestCase(2, Result = [|4; 6|])>]
[<TestCase(3, Result = [|6; 9; 4|])>]
[<TestCase(4, Result = [|6|])>]
[<TestCase(5, Result = [|1; 2; 4; 6|])>]
[<TestCase(100, Result = [|-1|], TestName = "Index out of range")>]
let ``Test 03: Nodes which have access to nodes (using MGraph)``ex =
  List.toArray(availableIn mgraph ex)

[<TestCase(1, Result = [|2; 4; 6|])>]
[<TestCase(2, Result = [|4; 6|])>]
[<TestCase(3, Result = [|6; 9; 4|])>]
[<TestCase(4, Result = [|6|])>]
[<TestCase(5, Result = [|1; 2; 4; 6|])>]
[<TestCase(100, Result = [|-1|], TestName = "Index out of range")>] 
let ``Test 04: Nodes which have access to nodes (using LGraph)``ex =
  List.toArray(availableIn lgraph ex)

[<TestCase(1, Result = [|-1|])>]
let ``Test 05: Nodes which have access to nodes (using EmptyLGraph)``ex =
  List.toArray(availableIn elgraph ex)
  
[<TestCase(1, Result = [|-1|])>]
let ``Test 06: Nodes which have access to nodes (using EmptyMGraph)``ex =
  List.toArray(availableIn emgraph ex)

[<TestCase(1, Result = [|-1|])>]
let ``Test 07: Nodes which available from  to nodes (using EmptyLGraph)``ex =
  List.toArray(availableFrom elgraph ex)

[<TestCase(1, Result = [|-1|])>]
let ``Test 08: Nodes which available from  to nodes (using EmptyMGraph)``ex =
  List.toArray(availableFrom emgraph ex)


[<EntryPoint>]
let main argv = 
    
    0 // return an integer exit code