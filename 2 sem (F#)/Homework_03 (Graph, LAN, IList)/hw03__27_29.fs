type IList<'A when 'A : equality> =
  interface
    abstract InsertHead : 'A -> unit
    abstract InsertTail : 'A -> unit
    abstract InsertTo   : 'A -> int -> unit
    abstract RemoveHead : unit -> unit
    abstract RemoveTail : unit -> unit
    abstract RemoveAt   : int -> unit
    abstract TryFind    : ('A -> bool) -> Option<'A>
    abstract Pop        : unit -> Option<'A>
    abstract Concat     : IList<'A> -> unit
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

      member this.InsertTo x index =  
        let rec f l i =
          match l with
          | Empty -> Node(x, Empty)
          | Node(a, b) -> if i = index then Node(x, l) else Node(a, f b (i+1))
        list <- (f list 1)
        
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
        if pos = 0 then (this :> IList<'A>).RemoveHead()
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

      member this.InsertHead x = array <- Array.append [|x|] array 

      member this.InsertTail x = array <- Array.append array [|x|]

      member this.InsertTo x index =           
        array <- Array.append (Array.append array.[0..(index - 1)] [|x|]) array.[index..(array.Length - 1)]

      member this.RemoveHead() =
        array <- Array.append array.[1..(array.Length - 1)] [||]

      member this.RemoveTail() =
        array <- Array.append array.[0..(array.Length - 2)] [||]

      member this.RemoveAt x  =
        array <- Array.append array.[0..(x - 1)] array.[(x + 1)..(array.Length - 1)]
    
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

[<EntryPoint>]
let main args =
  let exList1 = Node(1, Node(2, Node(6, Node(7, Empty))))
  let list = new ATDList<int>(exList1)
  printf "ATD list: " 
  list.PrintList() 
  printf "Insert 0 to head: "
  (list :> IList<int>).InsertHead 0
  list.PrintList()

  printf "Insert 1 to tail: "
  (list :> IList<int>).InsertTail 1
  list.PrintList()

  printf "Insert 2 to third index: "
  (list :> IList<int>).InsertTo 2 3
  list.PrintList()

  printf "Remove elem from head: "
  (list :> IList<int>).RemoveHead()
  list.PrintList()

  printf "Remove elem from tail: "
  (list :> IList<int>).RemoveTail()
  list.PrintList()

  printf "Remove fourth elem from list : "
  (list :> IList<int>).RemoveAt 3
  list.PrintList()

  printf "Find node divisible by 2 : "
  let x = (list :> IList<int>).TryFind (fun x -> x % 2 = 0 )
  match x with
  | None   -> printf "Not found\n"
  | Some x -> printf "%d\n" x

  let exList2 = Node(22, Node(33, Empty))
  let atdList = new ATDList<int>(exList2)
  printf "Second  ATDlist: " 
  atdList.PrintList()
  printf "Union of lists: "
  (list :> IList<int>).Concat atdList
  list.PrintList()
  printf "\n"

  let exArray1 = [|6 ; 5 ; 1 ; 3; 2|]
  let arList = new ArrayList<int>(exArray1)
  printf "Array list: " 
  arList.PrintArray()

  printf "Insert 4 to head: "
  (arList :> IList<int>).InsertHead 4
  arList.PrintArray()
  
  printf "Insert 0 to tail: "
  (arList :> IList<int>).InsertTail 0
  arList.PrintArray()
  
  printf "Insert 7 to fourth index: "
  (arList :> IList<int>).InsertTo 7 3
  arList.PrintArray()
  
  printf "Remove head from array: "
  (arList :> IList<int>).RemoveHead()
  arList.PrintArray()
  
  printf "Remove tail from array: "
  (arList :> IList<int>).RemoveTail()
  arList.PrintArray()
  
  printf "Remove fourth elem from array : "
  (arList :> IList<int>).RemoveAt 3
  arList.PrintArray()
  
  printf "Find node divisible by 2 : "
  let x = (arList :> IList<int>).TryFind (fun x -> x % 2 = 0 )
  match x with
  | None   -> printf "Not found\n"
  | Some x -> printf "%d\n" x

  let exArray2 = [|22; 33; 44|]
  let arList2 = new ArrayList<int>(exArray2)
  printf "Second array: " 
  arList2.PrintArray()
  printf "Union of arrays: "
  (arList :> IList<int>).Concat arList2
  arList.PrintArray()

  0    
    
 