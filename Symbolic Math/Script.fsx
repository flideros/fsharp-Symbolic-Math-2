#load "Statements.fs"
#load "Constants.fs"
#load "Operations.fs"
#load "Relations.fs"
#load "Objects.fs"
#load "Numbers.fs"

open MathObject
open Statements
open Constants
open Operations
open Number

// Construct math objects.
let x = Variable "x" |> Symbol 
let y = Variable "y" |> Symbol 

let one = Number (Integer 1I)
let two = Number (Integer 2I)
let oneN = Number (Natural 1UL)
let twoN = Number (Natural 2UL)
let oneHundredN = Number (Natural 100UL)

// sequence of natural numbers
let n100 = seq { for i in 0UL ..1UL..100UL -> Natural i }

// Get the value of a constant.
let getValue (x: Constant) = 
    match x with 
    | Pi p -> Constants.Pi.value
    | E e -> Constants.EulerNumber.value
    | _ -> 0.0

// Construct a constant.
let pi = (Constant.Pi Constants.Pi.value)

// Construct an operation.
let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
let times = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))

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

// Create an operation error
let binaryOpError (s: Set) (x1: Expression) (p: Operation) (x2: Expression) = Unknown |> Error |> Symbol

// Create operation service for a mathematical environment.
let ops = 
    {addition = Some NaturalNumbers.binaryAdd
     subtraction = None
     multiplication = Some NaturalNumbers.binaryMultiply
     division = None
     additiveInverse = None
     multiplicativeInverse = None}

// Create axioms for the algebraic structure
let asssociative = AssociativeAddition
let commutative = CommutativeAddition

// Create an mathematical structure
let testAlgebra = (Numbers n100, ops, NaturalNumbers.axioms) |> Algebraic 
let testAlgebra1 = (N, ops, NaturalNumbers.axioms) |> Algebraic

// Use addition operation from a mathematical structure
let testAdd alg = 
    match alg with
    | Algebraic (N,ops,_) when ops.addition.IsSome -> ops.addition.Value N
    | Algebraic (n100,ops,_) when ops.addition.IsSome -> ops.addition.Value n100
    | _ -> binaryOpError N

testAdd testAlgebra1 oneN plus (testAdd testAlgebra1 twoN plus oneN) // 4UL
testAdd testAlgebra1 one plus (testAdd testAlgebra1 twoN plus oneN) // binary op of an integer plus a natural number.
testAdd testAlgebra1 one plus (testAdd testAlgebra1 two plus one) // Symbol (Error OperationUndefined) since numbers are integers rather than natural numbers

testAdd testAlgebra oneN plus (testAdd testAlgebra twoN plus oneHundredN) // Returns Number (Natural 2UL) since testAlgebra is using n100 which has a length of 101
testAdd testAlgebra1 oneN plus (testAdd testAlgebra1 twoN plus oneHundredN) // Returns Number (Natural 103UL) since testAlgebra1 is using N (natural numbers)

// Check a sequence is Least Residue System
NaturalNumbers.isLeastResidueSystem (Numbers n100)

let testMult alg = 
    match alg with
    | Algebraic (N,ops,_) when ops.multiplication.IsSome -> ops.multiplication.Value N
    | Algebraic (n100,ops,_) when ops.multiplication.IsSome -> ops.multiplication.Value n100
    | _ -> binaryOpError N

testMult testAlgebra1 twoN times (testAdd testAlgebra1 twoN plus oneN) // 6UL

