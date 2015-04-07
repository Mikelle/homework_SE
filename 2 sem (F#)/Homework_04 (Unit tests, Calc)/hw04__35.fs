
open System
open System.Collections.Generic
open NUnit.Framework

type Stack<'A> () = 
  class
    let mutable stack : 'A list = []
    
    member this.Length = stack.Length
    member this.Push elm = stack <- elm::stack
    
    member this.Pop () =
      if stack.Length = 0 then failwith "Stack is empty"
      let res = stack.[0]
      stack <- stack.Tail
      res

    member this.Top () = 
      if stack.Length = 0 then failwith "Stack is empty"
      else stack.[0]

    override this.ToString () = sprintf "%A" stack
  end

let infix_to_postfix (str:string) =

  let prio c =
    match c with
    | '('            -> 0
    | '+' | '-'      -> 1
    | '*' | '/' | '%'-> 2
    | '^'            -> 3

  let stack = new Stack<char> ()

  let mutable postfix = ""

  for i in 0 .. str.Length - 1 do
    let cur_char =  str.[i] 
    
    if System.Char.IsDigit(cur_char)  then
      postfix <- postfix + cur_char.ToString()

    else
      
      match cur_char with
      | ' ' ->
        if System.Char.IsDigit(str.[i - 1])
        then postfix <- postfix + " "
      | '(' ->
          stack.Push(cur_char)
      | ')' -> 
        while stack.Length <> 0 && stack.Top() <> '(' do
          postfix <- postfix +  " " + stack.Pop().ToString() + " "
        ignore (stack.Pop())
      | _ -> 
        while stack.Length <> 0 && prio (stack.Top()) >= prio cur_char do
          postfix <-  postfix  + stack.Pop().ToString() + " "
        stack.Push(cur_char)
  while stack.Length <> 0 do postfix <- postfix + " " + stack.Pop().ToString() 
  postfix

let evaluate (expression:string) =

  let is_operator c =
    match c with
        | "+" | "-" | "*" | "/" | "%" | "^" -> true
        | _ -> false
 
  let calculate left operator right =
      match operator with
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | "/" -> left / right
        | "%" -> left % right
        | "^" -> left ** right
        | _ -> failwith "Unknown operator"
 
  let proc (item:string) (stack: Stack<float>) =
    if is_operator item then
      let r = stack.Pop()
      let l = stack.Pop()
      let result = calculate l item r
      stack.Push result
    else
      stack.Push(System.Convert.ToDouble(item))

  let stack = new Stack<double>()
  let items = expression.Split([|' '|])
  Array.iter (fun i -> proc i stack) items
  stack.Pop()

[<TestCase("19 + 18 * (2 + 6) / 3 ^ 2 % 3", Result = 20)>]
[<TestCase("(25 + 31 ^ 2 * 4 + 2) * 5", Result = 19355)>]
[<TestCase("1 - 2", Result = -1)>]
[<TestCase("2 * 2 / 2", Result = 2)>]
[<TestCase("1 + (6 / 2) ^ 2", Result = 10)>]
[<TestCase("555 % 500", Result = 55)>]
[<TestCase("(111 + 111) * 2", Result = 444)>]
[<TestCase("2 ^ 3 ^ 4", Result = 4096)>] 
let Test s = s |> infix_to_postfix |> evaluate
