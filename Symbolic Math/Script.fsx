#load "Constants.fs"
#load "Objects.fs"

open Constants
open MathObject


let x = Variable "x" |> Symbol 
let y = Variable "y" |> Symbol 

let one = Number (Integer 1I)
let two = Number (Integer 2I)

let getValue (x: Constant) = 
    match x with 
    | Pi p -> Constants.Pi.value

let pi = (Constant.Pi Constants.Pi.value)
let piSymbol = Constants.Pi.symbol
let eSymbol = Constants.EulerNumber.symbol


let add (s: Set) (x1: Expression) (x2: Expression)  = 
    match x1, x2, s with
    | Number (Integer x'), Number (Integer y'), Z -> (x' + y') |> Integer |> Number
    | _ -> (x1, Plus, x2, s) |> BinaryOp 


add Z two (add Z one x ) 
