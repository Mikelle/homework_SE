module main 

open System
open System.Collections.Generic
open System.ComponentModel
open System.Data
open System.Drawing
open System.Linq
open System.Text
open System.Windows.Forms
open System.Globalization

let mutable operand1 = ""
let mutable operand2 = ""
let mutable operation = ' '
let mutable res = ""


let programOutput =
  let textBox = new TextBox()
  textBox.Location <- System.Drawing.Point(0, 0)
  textBox.Height <- 240
  textBox.Width <- 240 
  textBox.Font <- new Font(textBox.Font.Name, (19.0F), textBox.Font.Style, textBox.Font.Unit) 
  textBox.Text <- "" 
  textBox.ReadOnly <- true
  textBox

let butDot =
  let but = new Button()
  but.Text <- "."
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(0, 120)
  but.Click.Add (fun _ ->
    if (not (programOutput.Text.Contains(".") || (programOutput.Text.Contains(","))))
    then programOutput.Text <- programOutput.Text + ".")
  but

let butPl =
  let but = new Button()
  but.Text <- "+"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 30)
  but.Click.Add(fun _ ->
    operand1 <- programOutput.Text
    operation <- '+'
    programOutput.Text <- "")
  but

let butMin =
  let but = new Button()
  but.Text <- "-"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 60)
  but.Click.Add(fun _ ->
    operand1 <- programOutput.Text
    operation <- '-'
    programOutput.Text <- "")
  but

let butMul =
  let but = new Button()
  but.Text <- "*"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 90)
  but.Click.Add(fun _ ->
    operand1 <- programOutput.Text
    operation <- '*'
    programOutput.Text <- "")
  but

let butDiv =
  let but = new Button()
  but.Text <- "/"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 120)
  but.Click.Add(fun _ ->
    operand1 <- programOutput.Text
    operation <- '/'
    programOutput.Text <- "")
  but

let butEq =
  let but = new Button()
  but.Text <- "="
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(80, 120)
  but.Click.Add(fun _ -> 
    let mutable opr1 = 0.0
    let mutable opr2 = 0.0
    operand2 <- programOutput.Text
    if (operand2 = "") || (operation = ' ')
    then operand1 |> ignore
    else
      opr1 <- Double.Parse(operand1, CultureInfo.InvariantCulture)
      opr2 <- Double.Parse(operand2, CultureInfo.InvariantCulture)
      match operation with
        | '+' ->
          res <- (opr1 + opr2).ToString() 
        | '-' ->
          res <- (opr1 - opr2).ToString()
        | '*' ->
          res <- (opr1 * opr2).ToString()
        | '/' ->
          if (opr2 <> 0.0)
          then res <- (opr1 / opr2).ToString()
          else MessageBox.Show("Can't divide by zero!") |> ignore
        | _-> MessageBox.Show("Unknown operator!") |> ignore
      programOutput.Text <- res
    operand1 <- ""
    operand2 <- ""
    operation <- ' '
      )
  but

let butClear =
  let but = new Button()
  but.Text <- "C"
  but.Size <- System.Drawing.Size(40, 30)             
  but.Location <- System.Drawing.Point(160, 30)
  but.Click.Add (fun _ ->
    programOutput.Text <- ""
    operand1 <- ""
    operand2 <- ""
    operation <- ' '
    )
  but

let butSqrt =
  let but = new Button()
  but.Text <- "√"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(160, 60)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (Math.Sqrt(!opr1).ToString())
    )
  but

let butSin =
  let but = new Button()
  but.Text <- "sin"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(200, 30)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (Math.Sin(!opr1).ToString())
    )
  but

let butCos =
  let but = new Button()
  but.Text <- "cos"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(200, 60)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (Math.Cos(!opr1).ToString())
    )
  but

let butTg =
  let but = new Button()
  but.Text <- "tan"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(200, 90)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (Math.Tan(!opr1).ToString())
    )
  but

let butCtg =
  let but = new Button()
  but.Text <- "ctan"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(200, 120)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (Math.Tan(!opr1).ToString())
    )
  but

let butPlMin =
  let but = new Button()
  but.Text <- "±"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(160, 90)
  but.Click.Add (fun _ ->
    let opr1 = ref 0.0
    if Double.TryParse(programOutput.Text, opr1)
    then programOutput.Text <- (!opr1 * -1.0).ToString()
    )
  but

let butDel = 
  let but = new Button()
  but.Text <- "←"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(160, 120)
  but.Click.Add (fun _ ->
    programOutput.Text <- programOutput.Text.[0..programOutput.Text.Length - 2])
  but

let mainForm =
  let form = new Form (Visible = false, TopMost = true)
  form.ClientSize <- Size(240, 150)

  for x in 0..9 do
    let but = new Button()
    but.Text <- x.ToString()
    but.Size <- System.Drawing.Size(40, 30)
    match x with 
    | 0 -> but.Location <- System.Drawing.Point(40, 120)
    | 1 -> but.Location <- System.Drawing.Point(0, 30)
    | 2 -> but.Location <- System.Drawing.Point(40, 30)
    | 3 -> but.Location <- System.Drawing.Point(80, 30)      
    | 4 -> but.Location <- System.Drawing.Point(0, 60)      
    | 5 -> but.Location <- System.Drawing.Point(40, 60)      
    | 6 -> but.Location <- System.Drawing.Point(80, 60)      
    | 7 -> but.Location <- System.Drawing.Point(0, 90)      
    | 8 -> but.Location <- System.Drawing.Point(40, 90)      
    | 9 -> but.Location <- System.Drawing.Point(80, 90)
    | _ -> failwith ""
      
    but.Click.Add(fun _ -> 
      if programOutput.Text = "0"
      then programOutput.Text <- x.ToString()
      else programOutput.Text <- programOutput.Text + x.ToString())
    form.Controls.Add(but)

  form.Controls.Add(butEq)
  form.Controls.Add(butPl)
  form.Controls.Add(butMin)
  form.Controls.Add(butMul)
  form.Controls.Add(butDiv)
  form.Controls.Add(butClear)
  form.Controls.Add(butDot)
  form.Controls.Add(butSqrt)
  form.Controls.Add(butSin)
  form.Controls.Add(butCos)
  form.Controls.Add(butTg)
  form.Controls.Add(butCtg)
  form.Controls.Add(butPlMin)
  form.Controls.Add(programOutput)
  form.Controls.Add(butDel)
  form
    

[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  Application.Run()
  0 // return an integer exit code
