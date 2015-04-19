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

     
[<EntryPoint>]
let main argv = 
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
    printfn "example: %A\n" example
    printfn "matrix:\n0 0 0 0 0 0 0\n0 0 1 0 0 0 1\n0 0 0 1 0 0 0\n0 0 1 0 0 0 0\n0 0 0 0 0 1 0\n1 0 0 0 0 0 0\n1 0 0 1 1 0 0" 
    printfn "arrayOfLists: %A\n" arrayOfLists 

    printf "Nodes which available from node 4 (using MGraph):\n"
    printf "%A\n\n" (availableFrom mgraph 4)
    printf "Nodes which available from node 4 (using LGraph list):\n"
    printf "%A\n\n" (availableFrom lgraph 4)

    printf "Nodes which have access to node 4 (using MGraph matrix):\n"
    printf "%A\n\n" (availableIn mgraph 4)
    printf "Nodes which have access to node 4 (using LGraph list):\n"
    printf "%A\n\n\n" (availableIn lgraph 4)
    0 // return an integer exit code
