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
open System.Text.RegularExpressions

let mutable operand1 = ""
let mutable operand2 = ""
let mutable operation = ' '
let mutable res = ""
let flagDec = ref true
let flagHex = ref false
let flagBin = ref false


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
  but.Text <- ","
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(0, 120)
  but.Click.Add (fun _ ->
    if not (programOutput.Text.Contains(","))
    then programOutput.Text <- programOutput.Text + ",")
  but

let butPl =
  let but = new Button()
  but.Text <- "+"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 30)
  but.Click.Add(fun _ ->
    if operand1 <> "" 
    then operation <- '+'
    else
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
    if operand1 <> "" 
    then operation <- '-'
    else
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
    if operand1 <> "" 
    then operation <- '*'
    else
      operand1 <- programOutput.Text
      operation <- '*'
      programOutput.Text <- ""
    )
  but

let butDiv =
  let but = new Button()
  but.Text <- "/"
  but.Size <- System.Drawing.Size(40, 30)
  but.Location <- System.Drawing.Point(120, 120)
  but.Click.Add(fun _ ->
    if operand1 <> "" 
    then operation <- '/'
    else
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
      if !flagBin || !flagHex 
      then
        MessageBox.Show("Calculator can do math only in decimal numbers.\nPress Dec button") |> ignore
        operand1 <- ""
        operand2 <- ""
        operation <- ' '
        programOutput.Text <- ""
      else
        opr1 <- Double.Parse(operand1)
        opr2 <- Double.Parse(operand2)
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
        programOutput.Text <-  res
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
    if programOutput.Text = "" then programOutput.Text <- programOutput.Text
    else programOutput.Text <- programOutput.Text.[0..programOutput.Text.Length - 2])
  but



let rec decButton  =
  let but = new Button()
  but.Text <- "Dec"
  but.Location <- Point(0, 150)
  but.Size <- Size(80, 30)
  but.Click.Add (fun _ ->
    if programOutput.Text <> ""
    then
      let output = ref 0
      if Int32.TryParse(programOutput.Text, System.Globalization.NumberStyles.HexNumber, null, output)
        && !flagHex
      then 
        programOutput.Text <- (!output).ToString()
        flagDec := true
        flagHex := false
      elif Regex.IsMatch(programOutput.Text, "^[01]+$") && !flagBin
      then 
        programOutput.Text <- (Convert.ToInt32(programOutput.Text, 2)).ToString()
        flagDec := true
        flagBin := false
      elif !flagDec
      then programOutput.Text <- programOutput.Text
    else 
      flagDec := true
      flagHex := false
      flagBin := false)
  but
  
let rec binButton =
  let but = new Button()
  but.Text <- "Bin"
  but.Location <- Point(80, 150)
  but.Size <- Size(80, 30)
  but.Click.Add (fun _ -> 
      
    let output = ref 0
    if programOutput.Text <> "" 
    then
      if Int32.TryParse(programOutput.Text, System.Globalization.NumberStyles.HexNumber, null, output)
        && !flagHex         
      then 
        programOutput.Text <- Convert.ToString(Int32.Parse((!output).ToString()), 2)
        flagBin := true
        flagHex := false
      elif Regex.IsMatch(programOutput.Text, "^[01]+$") && !flagBin
      then programOutput.Text <- programOutput.Text
      elif !flagDec 
      then 
        programOutput.Text <- Convert.ToString((Int32.Parse(programOutput.Text)), 2)
        flagBin := true
        flagDec := false
    else 
      flagBin := true
      flagHex := false
      flagDec := false)
  but

let rec hexButton =
  let but = new Button()
  but.Text <- "Hex"
  but.Size <- Size(80, 30)
  but.Location <- Point(160, 150)
  but.Click.Add (fun _ ->
    if programOutput.Text <> ""
    then
      let output = ref 0
      if Regex.IsMatch(programOutput.Text, "^[01]+$") && !flagBin
      then 
        programOutput.Text <- Convert.ToString(Int32.Parse((Convert.ToInt32(programOutput.Text, 2)).ToString()), 16)
        flagHex := true
        flagBin := false
      elif Int32.TryParse(programOutput.Text, System.Globalization.NumberStyles.HexNumber, null, output)
        && !flagHex
      then programOutput.Text <- programOutput.Text
      elif !flagDec
      then 
        programOutput.Text <- Convert.ToString((Int32.Parse(programOutput.Text)), 16)
        flagHex := true
        flagDec := false
      else  
        flagHex := true
        flagDec := false
        flagBin := false)
  but
  



let mainForm =
  let form = new Form (Visible = false, TopMost = true)
  form.ClientSize <- Size(240, 180)
  form.MaximumSize <- Size(256, 219)
  form.MinimumSize <- Size(256, 189)
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
  form.Controls.Add (decButton)
  form.Controls.Add (binButton)
  form.Controls.Add (hexButton)
  form
    

[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  Application.Run()
  0 // return an integer exit code