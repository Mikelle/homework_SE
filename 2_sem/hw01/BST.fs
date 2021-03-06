﻿// Binary Search Tree
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
      | Empty, Node (center1, left1, right1) -> Node (center1, left1, right1)
      | Node (center2, left2, right2), Empty -> Node(center2, left2, right2) 
      | left, Node(center2, Empty, right2) -> Node(center2, left, right2) 
      | left, Node(center2, left2, right2) -> Node(findMin left2, left, treeRemove (findMin left2) (Node(center2, left2, right2))) 
        
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

let rec printTree tree =
    match tree with
    | Empty -> ()
    | Node(center, left, right) ->
      printf "Node %i {" center
      match left with 
      | Empty -> printf "Leaf"
      | l -> printTree l
      printf "; "
      match right with
      | Empty -> printf "Leaf"
      | l -> printTree right
      printf "}"

[<EntryPoint>]
let main argv = 
    let tree = treeInsert 2 Empty 
    printf "insert 2: "
    printTree tree
    let tree = treeInsert 5 tree
    printf "\ninsert 5: "
    printTree tree 
    let tree = treeInsert 3 tree 
    printf "\ninsert 3: "
    printTree tree
    let tree = treeInsert 1 tree 
    printf "\ninsert 1: "
    printTree tree 
    let tree = treeInsert 4 tree 
    printf "\ninsert 4: "
    printTree tree 
    let tree = treeInsert 8 tree
    printf "\ninsert 8: "
    printTree tree
    let tree = treeInsert 9 tree
    printf "\ninsert 9: "
    printTree tree
    let tree = treeRemove 2 tree
    printf "\nremove 2: "
    printTree tree
    let tree = treeRemove 9 tree
    printf "\nremove 9: "
    printTree tree 
    printf "\n\nLCR: "
    tree |> printLCR
    printf "\nCLR: "
    tree |> printCLR
    printf "\nLCR: "
    tree |> printLRC
    0 

