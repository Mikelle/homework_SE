// Tasks 14-19
// Author: Wall Mikhail
// Expectation time: 2 hours, real time: 4 hours

// task 14: polymorphic tree type
type Tree<'A> = Empty | Node of 'A * Tree<'A> * Tree<'A>

let rec insert x tree =
    match tree with
    | Empty -> Node (x, Empty, Empty)
    | Node (center, left, right) ->
     match compare x center with
     | c when c < 0 -> Node (center, insert x left, right)
     | c when c > 0 -> Node (center, left, insert x right)
     | _ -> tree

let rec findMin x = 
  match x with 
  | Empty -> 0
  | Node (center, Empty, _) -> center
  | Node (center, left, _) -> findMin left

let rec treeRemove x tree =
  match tree with
  | Empty -> Empty
  | Node (center, left, right) ->
    match (compare x center) with
    | 1 -> Node (center, left, treeRemove x right) 
    | -1 -> Node (center, treeRemove x left, right) 
    | _->
      match left, right with 
      | Empty, Empty -> Empty
      | Empty, right -> right
      | left, Empty -> left 
      | left, Node(center1, Empty, right1) -> Node(center1, left, right1) 
      | left, Node(center2, left2, right2) -> 
        Node(findMin left2, left, treeRemove (findMin left2) (Node(center2, left2, right2))) 

let rec printLCR tree =
  match tree with 
  | Empty -> ()
  | Node (center, left, right) ->
    printLCR left
    printf "%d " center
    printLCR right

let rec printLRC tree = 
  match tree with 
  | Empty -> ()
  | Node (center, left, right) ->
    printLRC left
    printLRC right
    printf "%d " center

let rec printCLR tree = 
  match tree with 
  | Empty -> ()
  | Node (center, left, right) ->
    printf "%d " center
    printCLR left
    printCLR right

let rec print tree =
    match tree with
    | Empty -> ()
    | Node(center, left, right) ->
      printf "Node %A {" center
      match left with 
      | Empty -> printf "Leaf"
      | l -> print l
      printf "; "
      match right with
      | Empty -> printf "Leaf"
      | l -> print l
      printf "}"

// task 15: polymorphic map for tree
let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node (center, left, right) -> 
      Node (f center, map f left, map f right)

// task 16: polymorphic fold for tree
let rec fold f a tree =
    match tree with
    | Empty -> a
    | Node (center, left, right) -> 
      fold f (fold f (f a center) left) right

// task 17: sum of the tree
type Option<'A> = None | Some of 'A
 
let sum tree = fold (+) 0 tree

// task 18: min of the tree
let helpMin a b =
    match a with
    | None -> Some b
    | Some a -> Some (min a b)

let min tree = fold helpMin None tree

// task 19: copy tree
let rec copy tree = fold (fun x t -> insert t x) Empty tree

// make tree from list
let make list = List.fold (fun tree n -> insert n tree) Empty list

[<EntryPoint>]
let main argv =
    printfn "Tasks 14-19:\n"
    let l = [1..5]
    let ex = make l
    printfn "Tree type:\ntype Tree<'A> = Empty | Node of 'A * Tree<'A> * Tree<'A>\n"
    printfn "Tree for example:"
    ex |> print
    printfn "\n\ntree with increment by 1 using map:"
    map (fun a -> a + 1) ex |> print 
    printf "\n\nsum of numbers in tree using fold: "
    sum ex |> printfn "%A\n"
    min ex |> printfn "minimal number in tree: %A\n"
    printfn "copied tree:"
    copy ex |> print
    printfn ""

    0 
