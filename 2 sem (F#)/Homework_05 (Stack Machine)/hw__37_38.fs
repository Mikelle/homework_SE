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
  
let infix_to_postfix (inputFile: string) (outputFile : string) =
  use inputStream = new StreamReader(inputFile)
  use outputStream = new StreamWriter(outputFile)
  let str = inputStream.ReadToEnd()


  let prio c =
    match c with
    | '('            -> 0
    | '+' | '-'      -> 1
    | '*' | '/' | '%'-> 2
    | '^'            -> 3
    | _              -> -1

  let stack = new Stack<char> ()

  let mutable postfix = ""

  for i in 0 .. str.Length - 1 do
    let cur_char =  str.[i] 
    
    if System.Char.IsDigit(cur_char) then
      outputStream.WriteLine(cur_char)

    else
      
      match cur_char with
      | ' ' ->
        if System.Char.IsDigit(str.[i - 1])
        then postfix <- postfix + " "
      | '(' ->
          stack.Push(cur_char)
      | ')' -> 
        while stack.Length <> 0 && stack.Top() <> '(' do
          outputStream.WriteLine(stack.Pop())
        ignore (stack.Pop())
      | _ -> 
        while stack.Length <> 0 && prio (stack.Top()) >= prio cur_char do
          outputStream.WriteLine(stack.Pop())
        stack.Push(cur_char)
  while stack.Length <> 0 do 
    outputStream.WriteLine(stack.Pop())

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
    |_ -> stack.Push (System.Convert.ToDouble str)
  outstream.Write (stack.Top())

(*let writeVert (fin: StreamReader) (fout: StreamWriter) = 
  let st = infix_to_postfix (fin.ReadLine())
  for i in 0..st.Length - 1 do
    fout.Write("/n" + st.[i].ToString())
    *)
let read (filename : string) = 
  use stream = new StreamReader(filename)
  stream.ReadToEnd ()
  

let write (str:string) (filename : string)  = 
  use stream = new StreamWriter (filename)
  stream.Write str


[<TestCase ("4 ^ 3 ^ 3", Result = "4\r\n3\r\n^\r\n3\r\n^\r\n")>]
[<TestCase ("2 + 2 * 2", Result = "2\r\n2\r\n2\r\n*\r\n+\r\n")>]
[<TestCase ("7 * 6 + 5", Result = "7\r\n6\r\n*\r\n5\r\n+\r\n")>]
[<TestCase ("(7 + 5) * (2 + 3)", Result = "7\r\n5\r\n+\r\n2\r\n3\r\n+\r\n*\r\n")>]
let ``Test for 37th task`` (expression : string) =
    let inputFile = "test.in"
    let outputFile = "test.out"
    write expression inputFile
    infix_to_postfix inputFile outputFile
    let str = read outputFile
    str

[<TestCase ("5\n5\n+", Result = 10)>]
[<TestCase ("3\n4\n+\n2\n^\n5\n7\n-\n3\n^\n-", Result = 57)>]
[<TestCase ("3\n4\n-", Result = -1)>]
[<TestCase ("1024\n100\n%", Result = 24)>]
let ``Task 38`` x =
  write x "test.in"
  evaluate "test.in"
  let str = read "result.out"
  System.Convert.ToInt32 str
