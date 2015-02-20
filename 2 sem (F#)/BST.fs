// Binary Search Tree
// Main operations
// Author: Mikhail Wall

type Tree =
  | Empty
  | Node of int * Tree * Tree

let rec treeInsert x tree =
  match x, tree with 
  | x, Empty -> Node (x, Empty, Empty)
  | x, Node (center, left, right) ->
    if x = center
    then Node (x, left, right)
    else if x < center 
    then Node (center, treeInsert x left, right)
    else Node (center, left, treeInsert x right)

let rec findMin x = 
  match x with 
  | Empty -> 0
  | Node (center, Empty, _) -> center
  | Node (center, left, _) -> findMin left


let rec treeRemove x tree =
  match x, tree with
  | x, Empty -> Empty
  | x, Node (center, left, right) ->
    if x > center
    then Node (center, left, treeRemove x right) 
    else if x < center 
    then Node (center, treeRemove x left, right)
    else
      match left, right with 
      | Empty, Empty -> Empty
      | Empty, right -> right
      | left, Empty -> left
      | left, right -> 
        if left = Empty 
        then Node(findMin right, left, treeRemove (findMin right) right)
        else Node(findMin left, treeRemove (findMin left) left, right)

     

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
