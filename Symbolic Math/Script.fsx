#load "Statements.fs"
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
let pi = (Constant.Pi Constants.Pi.value)

// Construct an operation.
let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
let minus = Subtraction (Subtraction.Minus (Minus.symbol, Minus.opPosition, Binary))
let times = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
let divideBy = Division (Division.DivideBy (Divide.symbol, Divide.opPosition, Binary))
let pow = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let invA = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
let root = Root (Root.SquareRootOf (SquareRootOf.symbol, SquareRootOf.opPosition, Unary))

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

// test the isIrrational function

IrrationalNumbers.isIrrational (UnaryOp (root,Number (Integer 1842506251I),Z))


// test area
let quotient a b =
            match a, b with 
            | Number (Integer x), Number (Integer y) when y <> 0I && x = 0I -> Number (Integer 0I) // added 1/11/17
            | Number (Integer x), Number (Integer y) when y <> 0I ->
                let out = x/y
                let out' = 
                    match (x >= y && x >= 0I) with
                    | true -> out
                    | false -> 
                        match y < 0I || ((bigint.Abs x) - (bigint.Abs (out * y))) = 0I with 
                        | true -> out + 1I
                        | false -> out - 1I
                match 0I <= (x - out' * y) && (x - out' * y) <= (bigint.Abs y) - 1I with
                | true -> Number (Integer out')
                | false -> 
                    match x < 0I with 
                    | true -> Number (Integer (out' - 1I))
                    | false -> Number (Integer (out' + 1I))
            | _ -> Number Undefined

let remainder a b =
            match a, b with
            | Number (Integer x), Number (Integer y) when y <> 0I -> 
                match quotient a b with
                | Number (Integer q) -> Number (Integer (x - q*y))
                | _ -> Number Undefined
            | _ -> Number Undefined

let primes =
            let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
            let rec next x = seq{
                let test =
                   match x with
                   | Number (Integer x') when x' < 700I ->  IntegerNumbers.isPrimeNaive
                   | _ -> IntegerNumbers.isPrime
                match test x with
                | true when x = Number(Integer 2I) ->
                    yield Number(Integer 2I)
                    yield! next (Number(Integer 3I))
                | true -> yield x 
                          yield! next (IntegerNumbers.binaryAdd Z x plus (Number(Integer 2I)))
                | false -> yield! next (IntegerNumbers.binaryAdd Z x plus (Number(Integer 2I)))}
            next (Number (Integer 2I)) |> Seq.cache

let primesUpTo max = 
    let e x = 
        match x with 
        | Number (Integer i) -> i 
        | _ -> 1I
    Seq.takeWhile (fun x -> e x < max) primes



let factorCandidates n' =
    let n = 
        match n' with 
        | Number(Integer i) when i >= 0I-> i 
        | Number(Integer i) when i < 0I-> -i 
        | _ -> 0I
    let expand l =                
        let rec comb accLst elemLst =
            match elemLst with
            | h::t ->
                let next = [h]::List.map (fun el -> h::el) accLst @ accLst
                comb next t
            | _ -> accLst
        comb [] l 
        |> Seq.distinct
        |> Seq.toList   
    let rawCandidatesN = 
        let cand = Seq.choose (fun x -> match remainder n' x = Number(Integer 0I) with | true -> Some x | _ -> None) (primesUpTo (System.Numerics.BigInteger(System.Math.Sqrt(float n)) + 1I)) |> Seq.toList
        expand cand   
    List.map (fun x -> List.fold (fun x' acc -> match x' with | Number(Integer i) ->  Number(Integer (i*acc)) | _ -> Number(Integer acc) ) (Number(Integer 1I)) x) (rawCandidatesN |> List.map (fun x -> List.map ( fun x' -> match x' with | Number(Integer i) -> i) x))                                      
    |> Seq.distinct  
    |> Seq.toList

factorCandidates (Number(Integer 1842506251I))
