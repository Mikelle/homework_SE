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

let availableFrom (graph: IGraph<'A>) node =
  let visited = [|for i in 0..graph.Size - 1 -> false|]
  dfs graph visited node 0

let availableIn (graph: IGraph<'A>) node =
  let visited = [|for i in 0..graph.Size - 1 -> false|]
  dfs graph visited node 1

type ITGraph<'A, 'T> =
  interface
    inherit IGraph<'A>
    abstract taggs: 'T array
    abstract getTag: int -> 'T
  end

let example = [|5; 8; 3; 9; 2; 1; 4;|] 
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
let lgraph = new LGraph<int> (example, arrayOfLists)

[<Test>]  
let ``Test 01: Nodes which available from node 1 (using MGraph)``() =
  let a = availableFrom mgraph 1
  Assert.AreEqual([5], a )

[<Test>]  
let ``Test 02: Nodes which available from node 2 (using MGraph)``() =
  let a = availableFrom mgraph 2
  Assert.AreEqual([1; 5], a )

[<Test>]  
let ``Test 03: Nodes which available from node 3 (using MGraph)``() =
  let a = availableFrom mgraph 3
  Assert.AreEqual([9], a )

[<Test>]  
let ``Test 04: Nodes which available from node 4 (using MGraph)``() =
  let a = availableFrom mgraph 4
  Assert.AreEqual([5; 9; 3; 2; 1], a )

[<Test>]  
let ``Test 05: Nodes which available from node 5 (using MGraph)``() =
  let a = availableFrom mgraph 5
  Assert.AreEqual([], a )

[<Test>]  
let ``Test 06: Nodes which available from node 8 (using MGraph)``() =
  let a = availableFrom mgraph 8
  Assert.AreEqual([3; 9; 4; 5; 2; 1], a )

[<Test>]  
let ``Test 07: Nodes which available from node 9 (using MGraph)``() =
  let a = availableFrom mgraph 9
  Assert.AreEqual([3], a )

[<Test>]  
let ``Test 08: Nodes which available from node 1 (using LGraph)``() =
  let a = availableFrom lgraph 1
  Assert.AreEqual([5], a )

[<Test>]  
let ``Test 09: Nodes which available from node 2 (using LGraph)``() =
  let a = availableFrom lgraph 2
  Assert.AreEqual([1; 5], a )

[<Test>]  
let ``Test 10: Nodes which available from node 3 (using LGraph)``() =
  let a = availableFrom lgraph 3
  Assert.AreEqual([9], a )

[<Test>]  
let ``Test 11: Nodes which available from node 4 (using LGraph)``() =
  let a = availableFrom lgraph 4
  Assert.AreEqual([5; 9; 3; 2; 1], a )

[<Test>]  
let ``Test 12: Nodes which available from node 5 (using LGraph)``() =
  let a = availableFrom lgraph 5
  Assert.AreEqual([], a )

[<Test>]  
let ``Test 13: Nodes which available from node 8 (using LGraph)``() =
  let a = availableFrom lgraph 8
  Assert.AreEqual([3; 9; 4; 5; 2; 1], a )

[<Test>]  
let ``Test 14: Nodes which available from node 9 (using LGraph)``() =
  let a = availableFrom lgraph 9
  Assert.AreEqual([3], a )

[<Test>]  
let ``Test 15: Nodes which have access to node 1 (using MGraph)``() =
  let a = availableIn mgraph 1
  Assert.AreEqual([2; 4; 8], a )

[<Test>]  
let ``Test 16: Nodes which have access to node 2 (using MGraph)``() =
  let a = availableIn mgraph 2
  Assert.AreEqual([4; 8], a )

[<Test>]  
let ``Test 17: Nodes which have access to node 3 (using MGraph)``() =
  let a = availableIn mgraph 3
  Assert.AreEqual([8; 9; 4], a )

[<Test>]  
let ``Test 18: Nodes which have access to node 4 (using MGraph)``() =
  let a = availableIn mgraph 4
  Assert.AreEqual([8], a)

[<Test>]  
let ``Test 19: Nodes which have access to node 5 (using MGraph)``() =
  let a = availableIn mgraph 5
  Assert.AreEqual([1; 2; 4; 8], a)

[<Test>]  
let ``Test 20: Nodes which have access to node 8 (using MGraph)``() =
  let a = availableIn mgraph 8
  Assert.AreEqual([], a)

[<Test>]  
let ``Test 21: Nodes which have access to node 9 (using MGraph)``() =
  let a = availableIn mgraph 9
  Assert.AreEqual([3; 8; 4], a)

[<Test>]  
let ``Test 22: Nodes which have access to node 1 (using LGraph)``() =
  let a = availableIn lgraph 1
  Assert.AreEqual([2; 4; 8], a)

[<Test>]  
let ``Test 23: Nodes which have access to node 2 (using LGraph)``() =
  let a = availableIn lgraph 2
  Assert.AreEqual([4; 8], a)

[<Test>]  
let ``Test 24: Nodes which have access to node 3 (using LGraph)``() =
  let a = availableIn lgraph 3
  Assert.AreEqual([8; 9; 4], a)

[<Test>]  
let ``Test 25: Nodes which have access to node 4 (using LGraph)``() =
  let a = availableIn lgraph 4
  Assert.AreEqual([8], a)

[<Test>]  
let ``Test 26: Nodes which have access to node 5 (using LGraph)``() =
  let a = availableIn lgraph 5
  Assert.AreEqual([1; 2; 4; 8], a)

[<Test>]  
let ``Test 27: Nodes which have access to node 8 (using LGraph)``() =
  let a = availableIn lgraph 8
  Assert.AreEqual([], a)

[<Test>]  
let ``Test 28: Nodes which have access to node 9 (using LGraph)``() =
  let a = availableIn lgraph 9
  Assert.AreEqual([3; 8; 4], a)



[<EntryPoint>]
let main argv = 
    
    0 // return an integer exit code
