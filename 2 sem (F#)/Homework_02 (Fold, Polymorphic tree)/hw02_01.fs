// Tasks 9-13
// Author: Wall Mikhail
// Expectation time: 1 hour, real time: 1 hour

let rec fold f a l =
    match l  with 
    | [] -> a
    | x :: l -> fold f (f a x) l

// task 10: function for list reverse 
let reverse list = fold (fun acc elem -> elem::acc) [] list

// task 11: function for filter list
let filter f list = fold (fun acc elem -> if f(elem) then acc @ [elem] else acc) [] list

// task 12: function for map list
let map f list = fold (fun acc elem -> acc @ [f elem]) [] list

// task 13: horner's method
let horner x list = fold (fun acc elem -> elem + acc * x) 0 list

[<EntryPoint>]
let main argv =
    printfn "Tasks 9-13:\n"
    printfn "List.iter: val it : (('a -> unit) -> 'a list -> unit) = <fun:clo@1>\n"
    let ex = [0..10]
    ex |> printfn "list: %A\n"
    reverse ex |> printfn "reversed list: %A\n"
    filter (fun a -> (a % 2 = 0)) ex |> printfn "filter list: %A\n"
    map (fun a -> a * 2) ex |> printfn "multiplication by 2 using numbers from list: %A\n"
    horner 3 [2; -6; 2; -1] |> printfn "horner method:\nf(x) = 2x^3 - 6x^2 + 2x - 1 for x = 3\nf(3) = %A"

    0