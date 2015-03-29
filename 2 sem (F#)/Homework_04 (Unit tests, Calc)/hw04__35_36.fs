open NUnit.Framework

type Stack<'A> () = 
  class
    let mutable stack: 'A array = [||]
        
    member this.Length = stack.Length
    member this.Push elem = stack <- Array.append [|elem|] stack
        
    member this.Pop () =
      if stack.Length = 0 then failwith "Stack is empty"
        else
          let mutable t = [||]
          for i = stack.Length - 1 downto 1 do
              t <- Array.append [|stack.[i]|] t
          let res = stack.[0]
          stack <- t
          res

    member this.Top () = 
      if stack.Length = 0 then failwith "Stack is empty"
      else stack.[0]
  end

let prior operator =
  match operator with
  | '+' -> 1
  | '-' -> 1
  | '*' -> 2
  | '/' -> 2
  | '%' -> 2
  | '^' -> 3
  | _   -> 0

let isOperator (a : string) = 
  if a.Length = 1 && prior(a.[0]) > 0 then true else false

let stringToFloat (str : string) =
  let charToDouble (elem : char) = 
    System.Convert.ToDouble(System.Convert.ToInt32(elem) - 48)
  let mutable temp = 0.0
  let negative = 
    match str.[0] with
    | '-' -> true 
    | _   -> 
          temp <- charToDouble(str.[0])
          false
  for i = 1 to str.Length - 1 do
    temp <- temp * 10.0 + charToDouble(str.[i])
  if negative then -temp else temp

let convert (expression : string) =
  let stack = new Stack<char>()
  let res = new Stack<string>()
  let mutable str = expression
  let mutable j = 1
  let mutable pos = 0
  if str.[0] = 'x' then str <- str.Insert(0, "(")
  while j < str.Length do
    if System.Char.IsDigit(str.[j]) && (str.[j - 1] = ' ' || str.[j - 1] = '-')  
    then pos <- j - 1
    if str.[j] = ']' then str <- str.Insert(j + 1, ")")
    if str.[j] = 'x' then
      if str.[j - 1] = ' ' then str <- str.Insert(j, "(")
      if str.[j - 1] = '-' && (str.[j - 2] = ' ' || str.[j - 2] = '(') 
      then str <- str.Insert(j - 1, "(")
      if System.Char.IsDigit(str.[j - 1]) 
      then 
        str <- str.Insert(j, " * ")
        str <- str.Insert(pos, "(")
        j <- j + 4
      j <- j + 1
    j <- j + 1

  let mutable temp = ""
  for i = 0 to str.Length - 1 do
    let t = str.[i]
    if System.Char.IsDigit(t) || t = 'x' then temp <- temp + t.ToString()
    else
      match t with
      | ' ' ->
        if System.Char.IsDigit(str.[i - 1]) || str.[i - 1] = ']' then
          res.Push(temp)
          temp <- ""
      | '[' -> temp <- temp + t.ToString()
      | ']' -> temp <- temp + t.ToString()
      | '(' -> stack.Push(t)
      | ')' ->
        if temp.Length > 0 then
          res.Push(temp)
          temp <- "" 
        while stack.Top() <> '(' && stack.Length > 0 do
          res.Push(stack.Pop().ToString())
        ignore(stack.Pop())
      | _   ->
        if t = '-' && System.Char.IsDigit(str.[i + 1]) || str.[i + 1] = 'x' then 
          temp <- "-"
        else
          while stack.Length > 0 
            && (prior(stack.Top()) >= prior(t) && prior(t) < 3
              || (prior(stack.Top()) >  prior(t) && prior(t) = 3))
                do res.Push(stack.Pop().ToString())
          stack.Push(t)
            
  if temp.Length > 0 then
    res.Push(temp)
    temp <- "" 
  while stack.Length > 0 do res.Push(stack.Pop().ToString())
  res

let calculate expression (context : float array) =
  let stack = convert(expression)

  let analyse (str : string) =
    let getIndex (str : string) =
      let mutable temp = 0
      for i = 2 to str.Length - 2 do
        temp <- temp * 10 + System.Convert.ToInt32(str.[i]) - 48
      temp

    if str.[0] = 'x' then context.[getIndex str]
    else 
      if str.[0] = '-' && str.[1] = 'x' then 
        -1.0 * context.[getIndex (str.TrimStart('-'))]
      else stringToFloat str

  let rec apply operator =
    let mutable a = 0.0
    let mutable b = 0.0
    let mutable temp = stack.Pop()

    if isOperator temp
    then a <- apply temp
    else a <- analyse temp
    temp <- stack.Pop()
    if isOperator temp
    then b <- apply temp
    else b <- analyse temp
       
    match operator with
    | "+" -> a + b
    | "-" -> b - a
    | "*" -> a * b
    | "/" -> b / a
    | "%" -> b % a 
    | "^" ->
      let rec pow elem p =
        match p with
        | 0 -> 1.0
        | 1 -> elem
        | p -> elem * (pow elem (p - 1))
      if a >= 0.0 then pow b (System.Convert.ToInt32(a))
      else 1.0 / (pow b (-System.Convert.ToInt32(a)))
    | _   -> failwith "Incorrect operator"
            
  apply (stack.Pop())


[<TestCase ("1 + (-1)", Result = 0.0)>]
[<TestCase ("24 % 10", Result = 4.0)>]
[<TestCase ("(5 + 3) ^ 2 - 10", Result = 54.0)>]
[<TestCase ("2 / 3 * 3 - 2", Result = 0.0)>]

let ``Test 1: calculator without variables`` ex =
  calculate ex [||]

[<TestCase ("1 + x[0]", [|-1.0|], Result = 0.0)>]
[<TestCase ("x[0] % x[1]", [|24.0; 10.0|], Result = 4.0)>]
[<TestCase ("(5x[0] + 3) ^ 2 - 10", [|-2.0|], Result = 39.0)>]
[<TestCase ("2x[0] / 3 * 3x[1] - 2x[2] + 3 ^ 0", [|9.0; 2.0; 0.0|], Result = 37.0)>]
let ``Test 2: calculator with varaibles`` ex context =
    calculate ex context

[<EntryPoint>]
let main argv =
  0