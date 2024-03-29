﻿#load "Statements.fs"
#load "Constants.fs"
#load "Delimiters.fs"
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



let zero = Number (Integer 0I)
let one = Number (Integer 1I)
let two = Number (Integer 2I)
let twoNeg = Number (Integer -2I)
let oneN = Number (Natural 1UL)
let twoN = Number (Natural 2UL)
let oneHundredN = Number (Natural 100UL)
let minushalf = {numerator = -1I; denominator = 2I} |> Rational |> Number
let half = {numerator = 1I; denominator = 2I} |> Rational |> Number
let third = {numerator = 1I; denominator = 3I} |> Rational |> Number

// sequence of natural numbers
let n100 = seq { for i in 0UL ..1UL..100UL -> Natural i }
let even = seq { for i in 0UL ..2UL..100UL -> Natural i }
let iset = seq { minushalf; one; two; half; third }
NaturalNumbers.isLeastResidueSystem (Numbers even) // false
NaturalNumbers.isLeastResidueSystem (Numbers n100) // true

// Get the value of a constant.
let getValue (x: Constant) = 
    match x with 
    | Pi p -> Constants.Pi.value
    | E e -> Constants.EulerNumber.value
    | _ -> 0.0

// Construct a constant.
let pi = (Constant.Pi Constants.Pi.value) |> Symbol.Constant
let e = Constant.E Constants.EulerNumber.value |> Symbol.Constant

pi < x

RealNumbers.compareNumbers (Symbol e) (Symbol pi)

// Construct an operation.
let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
let sum = Addition (Addition.Sum (Sum.symbol, Sum.opPosition, Nary))
let product = Multiplication (Multiplication.Product (Product.symbol, Product.opPosition, Nary))
let minus = Subtraction (Subtraction.Minus (Minus.symbol, Minus.opPosition, Binary))
let times = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
let divideBy = Division (Division.DivideBy (Divide.symbol, Divide.opPosition, Binary))
let pow = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let invA = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
let root = Root (Root.SquareRootOf (SquareRootOf.symbol, SquareRootOf.opPosition, Unary))

pow > sum

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
     multiplicativeInverse = None
     toThePowerOf = None
     absoluteValue = None}

let rationalOps = 
    {addition = Some RationalNumbers.binaryAdd
     subtraction = Some RationalNumbers.binarySubtract
     multiplication = Some RationalNumbers.binaryMultiply
     division = None
     additiveInverse = None
     multiplicativeInverse = None
     toThePowerOf = None
     absoluteValue = None}

// Create axioms for the algebraic structure
let asssociative = AssociativeAddition
let commutative = CommutativeAddition

// Create an mathematical structure
let testAlgebra = (Numbers n100, ops, NaturalNumbers.axioms) |> Algebraic 
let testAlgebra1 = (N, ops, NaturalNumbers.axioms) |> Algebraic
let testAlgebra2 = (Numbers even, ops, NaturalNumbers.axioms) |> Algebraic
let testAlgebra3 = (Expressions iset, rationalOps, RationalNumbers.axioms) |> Algebraic

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
testAdd testAlgebra2 oneN plus twoN // Returns Symbol (Error NotInSet) since testAlgebra2 is using N (natural numbers)
testAdd testAlgebra2 twoN plus twoN // Returns Number (Natural 4UL)



// Check a sequence is Least Residue System
NaturalNumbers.isLeastResidueSystem (Numbers n100)

let testMult alg = 
    match alg with
    | Algebraic (N,ops,_) when ops.multiplication.IsSome -> ops.multiplication.Value N
    | Algebraic (n100,ops,_) when ops.multiplication.IsSome -> ops.multiplication.Value n100
    | _ -> binaryOpError N

testMult testAlgebra1 twoN times (testAdd testAlgebra1 twoN plus oneN) // 6UL

// test ToThePowerOf
IntegerNumbers.binaryPower Z twoNeg pow twoNeg

// test RationalNumbers binaryAdd etc
RationalNumbers.binaryAdd Q half plus half
RationalNumbers.binarySubtract Q third minus half
RationalNumbers.binaryMultiply Q third times half
RationalNumbers.binaryDivide Q half divideBy two

RationalNumbers.unaryAdditiveInverse Q addativeInverse  half
RationalNumbers.unaryMultiplicativeInverse Q multiplicativeInverse third

// test RationalNumbers binaryAdd on iset
RationalNumbers.binaryAdd (Expressions iset) half plus half
RationalNumbers.binarySubtract (Expressions iset) one minus half
RationalNumbers.binaryMultiply (Expressions iset) half times half

IntegerNumbers.binarySubtract Z one minus one

// isIrrational function
RealNumbers.isIrrational (UnaryOp (root,Number (Integer 1040604010000000011I),Z))

RealNumbers.compareNumbers  (UnaryOp (root,Number (Integer 10I),Z)) (Symbol pi)

// Square number test
IntegerNumbers.isSquare (Integer 1040604010000000011I)

// working with prime numbers
for n in IntegerNumbers.primesUpToCount 100 do printfn "%A" n
for n in IntegerNumbers.primesUpTo 100I do printfn "%A" n

// Primality test
IntegerNumbers.isPrime (Integer 1040604010000000011I)

// RealNumber evaluateRealExpression function
let testExp1 = BinaryOp (Number (Integer 1I), plus, Number (Decimal 2.02M), R)
let testExp2 = BinaryOp ((BinaryOp (Number (Integer 1I), plus, Number (Decimal 2.02M), R)), plus, Number (Decimal 2.02M), R)
let testExp3 = BinaryOp ((BinaryOp (Number (Integer 5I), times, Number (Decimal 2.02M), R)), plus, Number (Decimal 2.02M), R)

RealNumbers.evaluateRealExpression testExp3

let g = Number(Real nan)
g.ToString() 

let testNaryOp1 = NaryOp (sum,[Number(Decimal 2.02M);Number(Decimal 7.32M);Number(Decimal 6.52M)],R)
let testNaryOp2 = NaryOp (sum,[Number(Real 2.02);Number(Real 4.02);Number(Real 3.32)],R)
RealNumbers.evaluateRealExpression testNaryOp1 // Number (Real 15.86)
RealNumbers.evaluateRealExpression testNaryOp2 // Number (Real 9.36)
RealNumbers.getRealValue testNaryOp1 // nan since its a list of Decimals. The function evaluateRealExpression recurses the atoms through this function first.
RealNumbers.getRealValue testNaryOp2 // Number (Real 9.36)

let testNaryOp3 = NaryOp (product,[Number(Decimal 2.02M);Number(Decimal 7.32M);Number(Decimal 6.52M)],R)
let testNaryOp4 = NaryOp (product,[Number(Real 2.02);Number(Real 4.02);Number(Real 3.32)],R)
RealNumbers.evaluateRealExpression testNaryOp3 // Number (Real 96.407328)
RealNumbers.evaluateRealExpression testNaryOp4 // Number (Real 26.959728)
RealNumbers.getRealValue testNaryOp3 // nan since its a list of Decimals. The function evaluateRealExpression recurses the atoms through this function first.
RealNumbers.getRealValue testNaryOp4 // Number (Real 26.959728)


let testNaryOp5 = NaryOp (product,[Number(Real 2.02);Number(Real 4.02);NaryOp (sum,[Number(Decimal 2.02M);Number(Decimal 7.32M);Number(Decimal 6.52M)],R)],R)
RealNumbers.evaluateRealExpression testNaryOp5 // Number (Real 128.789544)