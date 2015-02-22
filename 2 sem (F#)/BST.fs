// Binary Search Tree
// Main operations
// Author: Mikhail Wall
// Expectation time: 1 hour, real time: 2 hours

type Tree =
  | Empty
  | Node of int * Tree * Tree

let rec treeInsert x tree =
  match x, tree with 
  | x, Empty -> Node (x, Empty, Empty)
  | x, Node (center, left, right) ->
    if x < center 
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

[<EntryPoint>]
let main argv = 
    let tree = treeInsert 2 Empty 
    let tree = treeInsert 5 tree 
    let tree = treeInsert 3 tree 
    let tree = treeInsert 1 tree 
    let tree = treeInsert 4 tree 
    let tree = treeInsert 8 tree 
    let tree = treeInsert 9 tree
    let tree = treeRemove 2 tree
    let tree = treeRemove 9 tree 
    tree |> printLCR
    printf "LCR\n"
    tree |> printCLR
    printf "CLR\n"
    tree |> printLRC
    printf "LCR\n"
    0 
