module Operations

open MathObject

let add (s:Set) (x1: Expression) (x2: Expression)  = 
    match x1, x2, s with
    | Number (Integer x'), Number (Integer y'), Z -> (x' + y') |> Integer |> Number
    | _ -> (x1, Plus, x2, s) |> BinaryOp 