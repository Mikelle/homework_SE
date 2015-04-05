open System
open System.Collections.Generic
open NUnit.Framework
open System.IO

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

    member this.GetList () = stack

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
    
    if System.Char.IsDigit(cur_char) then
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

let evaluate (str: string) = 
  use instream = new StreamReader "test.in"
  use outstream = new StreamWriter "result.out"
  let stack = new Stack<float>()
  while not instream.EndOfStream do 
    let str = instream.ReadLine()
    match str with 
    | "+" ->
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b + a)
    | "-" -> 
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b - a)
    | "*" -> 
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b * a)
    | "/" ->
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b / a)
    | "%" -> 
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b % a)
    | "^" -> 
      let a = stack.Pop()
      let b = stack.Pop() 
      stack.Push(b ** a)
    | _ -> stack.Push (System.Convert.ToDouble str)
  outstream.Write (stack.Top())

let writeVert (fin: StreamReader) (fout: StreamWriter) = 
  let st = infix_to_postfix (fin.ReadLine())
  for i in 0..st.Length - 1 do
    fout.Write(st.[i].ToString())

let read (filename : string) = 
  use stream = new StreamReader(filename)
  let str = stream.ReadToEnd ()
  str

let write (filename : string) (str:string) = 
  use stream = new StreamWriter (filename)
  stream.Write str


[<TestCase ("10 + 11", Result = "10 11 +")>]
[<TestCase ("4 ^ 3 ^ 3", Result = "4 3 ^ 3 ^")>]
[<TestCase ("2 + 2 * 2", Result = "2 2 2 * +")>]
[<TestCase ("7 * 6 + 5", Result = "7 6 * 5 +")>]
let ``Task 37`` (x: string) =
  write "test.in" x
  let w = new StreamWriter("test.in") 
  w.Write(x)
  w.Dispose()
  let sout = new StreamWriter("test.out")
  let sin = new StreamReader("test.in")
  writeVert sin sout
  sin.Dispose()
  sout.Dispose()
  let res = read("test.out")
  res

[<TestCase ("5\n5\n+", Result = 10)>]
[<TestCase ("3\n4\n+\n2\n^\n5\n7\n-\n3\n^\n-", Result = 57)>]
[<TestCase ("3\n4\n-", Result = -1)>]
[<TestCase ("1024\n100\n%", Result = 24)>]
let ``Task 38`` x =
  write "test.in" x
  evaluate "test.in"
  let str = read "result.out"
  System.Convert.ToInt32 str