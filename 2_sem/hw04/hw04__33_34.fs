open NUnit.Framework

type IList<'A when 'A : equality> =
  interface
    abstract InsertHead : 'A -> unit
    abstract InsertTail : 'A -> unit
    abstract InsertTo   : 'A -> int -> bool
    abstract RemoveHead : unit -> unit
    abstract RemoveTail : unit -> unit
    abstract RemoveAt   : int -> bool
    abstract TryFind    : ('A -> bool) -> Option<'A>
    abstract Pop        : unit -> Option<'A>
    abstract Concat     : IList<'A> -> unit
    abstract Length     : unit -> int
  end

type List<'A> = Empty | Node of 'A * List<'A>

type ATDList<'A when 'A : equality>(L : List<'A>) =
  class
    let mutable list = L

    member this.GetList() = list

    member this.PrintList() = 
      let rec f l =
        match l with
        | Empty -> ()
        | Node(a, b) -> 
          printf "%A " a
          f b
      f list
      printf "\n"

    interface IList<'A> with

      member this.InsertHead x  = 
        list <- Node(x, list)

      member this.InsertTail x =
        let rec f l =
          match l with
          | Empty -> Node(x, Empty)
          | Node(a, b) -> Node(a, f b)
        list <- f list

      member this.Length() =
        let rec f l length =
          match l with 
          | Empty -> length 
          | Node(_, b) -> f b (length + 1)
        f list 0

      member this.InsertTo x index = 
        if index < 1 || index > (this :> IList<'A>).Length() + 1 then false 
        else
          let rec f l i =
            match l with
            | Empty -> Node(x, Empty)
            | Node(a, b) -> if i = index then Node(x, l) else Node(a, f b (i+1))
          list <- f list 1
          true

      member this.RemoveHead() =
        match list with
        | Empty -> ()
        | Node(a, b) -> list <- b

      member this.RemoveTail() =
        let rec f l =
          match l with
          | Empty -> Empty
          | Node(_, Empty) -> Empty
          | Node(a, b) -> Node(a, f b)
        list <- f list
        

      member this.RemoveAt pos =
        if pos < 1 || pos > (this :> IList<'A>).Length() then false
        else
          let rec f n l =
            match l with
              | Empty -> Empty
              | Node (a, b) ->
                if n = pos - 1 then 
                  match b with
                  | Empty -> Empty
                  | Node (_, b') -> Node (a, b')
                else Node (a, f (n + 1) b)
          list <- f 0 list
          true

      member this.TryFind x =
        let rec f l =
          match l with
          | Empty -> None
          | Node(a, b) ->
            if x a then Some(a) else (f b)
        f list

      member this.Pop() =
        match list with
        | Empty -> None 
        | Node(a, _) ->
          (this :> IList<'A>).RemoveHead() 
          Some a

      member this.Concat lst  =
        let rec f l =
          match l with
          | Node(a, b) -> Node(a, f b)
          | Empty -> 
            match lst.Pop() with
            | None -> Empty
            | Some a -> Node(a, f l)
        list <- f list
  end


type ArrayList<'A when 'A : equality>(L : 'A[]) =
  class
    let mutable array = L

    member this.GetArray() = array

    member this.PrintArray() = 
      Array.iter (fun x -> printf "%A " x) array
      printf "\n"

    interface IList<'A> with

      member this.Length() = Array.length array

      member this.InsertHead x = array <- Array.append [|x|] array 

      member this.InsertTail x = array <- Array.append array [|x|]

      member this.InsertTo x pos = 
        let length = Array.length array
        if pos < 0 || pos > length  then false
        else          
          array <- Array.append (Array.append array.[0..(pos - 1)] [|x|]) array.[pos..(array.Length - 1)]
          true

      member this.RemoveHead() =
        array <- Array.append array.[1..(array.Length - 1)] [||]

      member this.RemoveTail() =
        let length = Array.length array
        if length < 1 then  array <- [||]
        else
          array <- Array.append array.[0..(array.Length - 2)] [||]
          

      member this.RemoveAt pos  =
        let length = Array.length array
        if pos < 1 || pos > length || length < 1 then false
        else
          array <- Array.append array.[0..(pos - 1)] array.[(pos + 1)..(array.Length - 1)]
          true

      member this.TryFind x = Array.tryFind x array

      member this.Pop() =
        match array with
        | [||] -> None
        | _ -> 
          let temp = array.[0]
          (this :> IList<'A>).RemoveHead()
          Some temp

      member this.Concat arr = 
        let rec f (arr: IList<'A>) =
          match arr.Pop() with 
          | None -> ()
          | Some a ->
            array <- Array.append array [|a|]
            f arr
        f arr
  end

let listToArr ex = 
  let f() = 
    match (ex :> IList<'A>).Pop() with
    | None -> 
      failwith "Wrong test result"
    | Some x -> x
  let length = ex.Length()
  Array.map (fun x -> f() ) [|1..length|]

[<Test>] 
let ``Insert Head in ATDList`` ()=
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  exList.InsertHead 0
  Assert.AreEqual([|0; 1; 2|], listToArr exList)

[<Test>] 
let ``Insert Tail in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  exList.InsertTail 3
  Assert.AreEqual([|1; 2; 3|], listToArr exList)

[<Test>]
let ``Insert Head in empty ATDList`` () =
  let list = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  exList.InsertHead 0
  Assert.AreEqual([|0|], listToArr exList)

[<Test>]
let ``Insert Tail in empty ATDList`` () =
  let list = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  exList.InsertHead 1
  Assert.AreEqual([|1|], listToArr exList)

[<Test>]
let ``Insert to x in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.InsertTo 3 2
  Assert.AreEqual(true, a)
  Assert.AreEqual([|1; 3; 2|], listToArr exList) 

[<Test>]
let ``Insert to wrong index in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.InsertTo 3 5
  Assert.AreEqual(false, a)

[<Test>]
let ``Insert to empty ATDList`` () =
  let list  = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.InsertTo 9 1
  Assert.AreEqual(true, a)
  Assert.AreEqual([|9|], listToArr exList)

[<Test>] 
let ``Remove Head in ATDList`` ()=
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  exList.RemoveHead() 
  Assert.AreEqual([|2|], listToArr exList)

[<Test>] 
let ``Remove Tail in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  exList.RemoveTail()
  Assert.AreEqual([|1|], listToArr exList)

[<Test>]
let ``Remove Head in empty ATDList`` () =
  let list = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  exList.InsertHead 0
  Assert.AreEqual([|0|], listToArr exList)

[<Test>]
let ``Remove Tail in empty ATDList`` () =
  let list = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  exList.RemoveHead()
  Assert.AreEqual([||], listToArr exList)

[<Test>]
let ``Remove from x in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.RemoveAt 2
  Assert.AreEqual(true, a)
  Assert.AreEqual([|1|], listToArr exList) 

[<Test>]
let ``Remove from wrong index in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.RemoveAt 5
  Assert.AreEqual(false, a)

[<Test>]
let ``Remove elem from empty ATDList`` () =
  let list  = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.RemoveAt 1
  Assert.AreEqual(false, a)
 
[<Test>]
let ``Find in ATDList`` () =
  let list = Node(1, Node(2, Empty))
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.TryFind (fun x -> if (x % 2) = 0 then true else false)
  Assert.AreEqual(Some 2, a)

[<Test>]
let ``Find in empty ATDList`` () =
  let list = Empty
  let exList = new ATDList<int>(list) :> IList<int>
  let a = exList.TryFind (fun x -> if (x % 2) = 0 then true else false)
  Assert.AreEqual(None, a)

[<Test>]
let ``Contact ATDLists`` () =
  let l1 = Node(1, Node(2, Empty))
  let l2 = Node(9, Node(8, Empty))
  let ex1 = new ATDList<int>(l1) :> IList<int>
  let ex2 = new ATDList<int>(l2) :> IList<int>
  ex1.Concat ex2
  Assert.AreEqual([|1; 2; 9; 8|], listToArr ex1)

[<Test>]
let ``Contact empty ATDList with not empty ATDList`` () =
  let l1 = Empty
  let l2 = Node(9, Node(8, Empty))
  let ex1 = new ATDList<int>(l1) :> IList<int>
  let ex2 = new ATDList<int>(l2) :> IList<int>
  ex1.Concat ex2
  Assert.AreEqual([|9; 8|], listToArr ex1)

[<Test>] 
let ``Insert Head in ArrayList`` ()=
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.InsertHead 0
  Assert.AreEqual([|0; 1; 2|], listToArr ex)

[<Test>] 
let ``Insert Tail in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.InsertTail 3
  Assert.AreEqual([|1; 2; 3|], listToArr ex)

[<Test>]
let ``Insert Head in empty ArrayList`` () =
  let arr = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.InsertHead 0
  Assert.AreEqual([|0|], listToArr ex)

[<Test>]
let ``Insert Tail in empty ArrayList`` () =
  let arr = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.InsertHead 1
  Assert.AreEqual([|1|], listToArr ex)

[<Test>]
let ``Insert to x in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.InsertTo 3 2
  Assert.AreEqual(true, a)
  Assert.AreEqual([|1; 2; 3|], listToArr ex) 

[<Test>]
let ``Insert to wrong index in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.InsertTo 3 5
  Assert.AreEqual(false, a)

[<Test>]
let ``Insert to empty ArrayList`` () =
  let arr  = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.InsertTo 1 0
  Assert.AreEqual(true, a)
  Assert.AreEqual([|1|], listToArr ex)

[<Test>] 
let ``Remove Head in ArrayList`` ()=
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.RemoveHead() 
  Assert.AreEqual([|2|], listToArr ex)

[<Test>] 
let ``Remove Tail in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.RemoveTail()
  Assert.AreEqual([|1|], listToArr ex)

[<Test>]
let ``Remove Head in empty ArrayList`` () =
  let arr = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  ex.InsertHead 0
  Assert.AreEqual([|0|], listToArr ex)

[<Test>]
let ``Remove from x in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.RemoveAt 1
  Assert.AreEqual(true, a)
  Assert.AreEqual([|1|], listToArr ex) 

[<Test>]
let ``Remove from wrong index in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.RemoveAt 5
  Assert.AreEqual(false, a)

[<Test>]
let ``Remove elem from empty ArrayList`` () =
  let arr  = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.RemoveAt 1
  Assert.AreEqual(false, a)
 
[<Test>]
let ``Find in ArrayList`` () =
  let arr = [|1; 2|]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.TryFind (fun x -> if (x % 2) = 0 then true else false)
  Assert.AreEqual(Some 2, a)

[<Test>]
let ``Find in empty ArrayList`` () =
  let arr = [||]
  let ex = new ArrayList<int>(arr) :> IList<int>
  let a = ex.TryFind (fun x -> if (x % 2) = 0 then true else false)
  Assert.AreEqual(None, a)

[<Test>]
let ``Contact ArrayList`` () =
  let arr1 = [|1; 2|]
  let arr2 = [|9; 8|]
  let ex1 = new ArrayList<int>(arr1) :> IList<int>
  let ex2 = new ArrayList<int>(arr2) :> IList<int>
  ex1.Concat ex2
  Assert.AreEqual([|1; 2; 9; 8|], listToArr ex1)

[<Test>]
let ``Contact empty ArrayList with not empty ArrayList`` () =
  let arr1 = [||]
  let arr2 = [|9; 8|]
  let ex1 = new ArrayList<int>(arr1) :> IList<int>
  let ex2 = new ArrayList<int>(arr2) :> IList<int>
  ex1.Concat ex2
  Assert.AreEqual([|9; 8|], listToArr ex1)

[<Test>]
let ``Contact ATDList with ArrayList`` () =
  let l = Node(1, Node(2, Empty))
  let ar = [|3; 4|]
  let ex1 = new ATDList<int>(l) :> IList<int>
  let ex2 = new ArrayList<int>(ar) :> IList<int>
  ex1.Concat ex2
  Assert.AreEqual([|1; 2; 3; 4|], listToArr ex1)

[<EntryPoint>]
let main args =

  0    