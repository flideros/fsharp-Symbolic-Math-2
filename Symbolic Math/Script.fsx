﻿#load "Constants.fs"
#load "Operations.fs"
#load "Objects.fs"

open Constants
open MathObject
open Operations

// Construct math objects.
let x = Variable "x" |> Symbol 
let y = Variable "y" |> Symbol 

let one = Number (Integer 1I)
let two = Number (Integer 2I)

// Get the value of a constant.
let getValue (x: Constant) = 
    match x with 
    | Pi p -> Constants.Pi.value
    | E e -> Constants.EulerNumber.value

// Construct a constant.
let pi = (Constant.Pi Constants.Pi.value)

// Construct an operation.
let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))

// Get symbol string for a constant.
let piSymbol = Constants.Pi.symbol
let eSymbol = Constants.EulerNumber.symbol

// Example of a function that takes a set and performs an operation over that set.
let add (s: Set) (x1: Expression) (p: Operation) (x2: Expression)  = 
    match x1, x2, s with
    | Number (Integer x'), Number (Integer y'), Z -> (x' + y') |> Integer |> Number
    | _ -> (x1, p, x2, s) |> BinaryOp 

add Z two plus (add Z one plus x) 
add Z two plus two

// Create operation service for a mathematical environment.
let ops = 
    {addition = Some add
     subtraction = None
     multiplication = None
     division = None
     additiveInverse = None
     multiplicativeInverse = None}

// Create axioms for the algebraic structure
let asssociative = AssociativeAddition
let commutative = CommutativeAddition

// Create an mathematical structure
let testAlgebra = Algebraic (Z, ops, [asssociative;commutative]) 