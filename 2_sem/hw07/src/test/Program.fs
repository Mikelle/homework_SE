open Workflow
open NUnit.Framework

//test for 40 task
[<TestCase(5, Result = 0)>]
[<TestCase(6, Result = 4)>]
let Ring n =
  ring n {
    let! a = 2 * 3
    let! b = 4
    return a + b
  }

//test for 41 task
[<Test>]
let ``map tree``() = 
  let tree = Node(3, Node(1, Empty, Node(2, Empty, Empty)), Empty)
  let res = map (fun x -> x + 2) tree
  Assert.AreEqual (Node(5, Node(3, Empty, Node(4, Empty, Empty)), Empty), res) 

[<Test>]
let ``map empty tree``() =
  let tree = Empty 
  let res = map (fun x -> x + 1) tree
  let (ans:Tree<int>) = Empty
  Assert.AreEqual(ans, res)

[<Test>]
let ``filter tree``() = 
  let tree = Node(2, Node(3, Node(4, Empty, Empty), Node(6, Empty, Empty)), Node(5, Empty, Empty))
  let res = filter (fun x -> x = 2 ) tree
  Assert.AreEqual(Node(2, Empty, Empty), res)

[<Test>]
let ``filter empty tree``() =
  let tree = Empty
  let res = filter (fun x -> (x % 2) = 1) tree 
  let (ans:Tree<int>) = Empty
  Assert.AreEqual (ans, res)

[<Test>]
let ``calc tree``() =
  let t1 = Node(2, Empty, Empty)
  let t2 = Node(4, Empty, Empty)
  let res = calc (*) t1 t2
  Assert.AreEqual (Node(8, Empty, Empty), res)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
