module Workflow

//task 40
type RingBuilder (n:int) =
  member this.Bind(x, f) = f (x % n)
  member this.Return(x) = x % n

let ring n = RingBuilder(n)

//task 41
type Tree<'A> = Empty | Node of 'A * Tree<'A> * Tree<'A>

let rec insert tree x =
    match tree with
    | Empty -> Node (x, Empty, Empty)
    | Node (center, left, right) ->
     match compare x center with
     | c when c < 0 -> Node (center, insert left x, right)
     | c when c > 0 -> Node (center, left, insert right x)
     | _ -> Node (center, left, right)

let rec fold f1 f2 acc tree =
    match tree with
    | Empty -> acc
    | Node (center, left, right) -> 
      fold f1 f2 (fold f1 f2 (f1 (acc) (f2 center)) left) right

let merge t1 t2 = fold insert (fun x -> x) t1 t2

type TreeBuilder () =
  member this.Bind(x, f) = fold merge f Empty x
  member this.Return(x) = Node(x, Empty, Empty)
  member this.ReturnFrom(x) = x
  member this.Yield(x) = Node(x, Empty, Empty)
  member this.YieldFrom(x) = x
  member this.Combine(x, y) = merge x y
  member this.For(x, f) = this.Bind(x, f)
  member this.Run(f) = f()
  member this.Delay(f) = f
  member this.Zero() = this.Yield ()

let treeWorkflow = TreeBuilder()

let map f tree =
  treeWorkflow {
    for i in tree do
      yield f i
  }

let filter с tree =
  treeWorkflow {
    for i in tree do
      if с i then yield i else yield! Empty
  }

let calc op x y =
  treeWorkflow {
    for i in x do
    for j in y do
    yield op i j
  }

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

