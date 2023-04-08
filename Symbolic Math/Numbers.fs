namespace MathObject

open System.Numerics
open Statements
open Relations
open Operations

module NaturalNumbers =     
(*
The system of natural numbers is a set N = {0; 1; 2; 3; ...} together with three binary relations,
addition, multiplication, order relation and axioms governing their interaction.
*)    
    let set = N
    let leastResidueSystemSet modulus = seq { for i in 0UL .. modulus -> Natural i }
    let leastResidueSystemExpressionSet modulus = seq { for i in 0UL .. modulus -> Natural i |> Number}
    
    
    let axioms =
        [AssociativeAddition; CommutativeAddition; AdditiveIdentity; AdditiveCancellation;
         AssociativeMultiplication; CommutativeMultiplication; MultiplicativeIdentity; MultiplicativeCancellation;
         Distributive; Induction; Successor; Transitivity; Antisymmetry]
    let compare this that = 
        match this, that with
        | Natural x, Natural y when x > y -> GreaterThan |> Relation |> Symbol
        | Natural x, Natural y when x < y -> LessThan |> Relation |> Symbol
        | Natural x, Natural y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let compareExpressions this that = 
        match this, that with
        | Number (Natural x), Number (Natural y) when x > y -> GreaterThan |> Relation |> Symbol
        | Number (Natural x), Number (Natural y) when x < y -> LessThan |> Relation |> Symbol
        | Number (Natural x), Number (Natural y) when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let isLeastResidueSystem s = 
        match s with
        | N -> true
        | Numbers n -> //false
            let m = Seq.length n - 1 |> uint64
            let comp a b = match compare a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (n,(leastResidueSystemSet m)) ||> Seq.compareWith comp = 0
        | Expressions e -> //false
            let m = Seq.length e - 1 |> uint64
            let comp a b = match compareExpressions a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (e,(leastResidueSystemExpressionSet m)) ||> Seq.compareWith comp = 0
        | _ -> false


    let binaryAdd s e1 op e2 =
        match s, op with
        | N, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) -> Natural (a + b) |> Number
            | Number (Natural a), _ -> (e1,op,e2,s) |> BinaryOp
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) && 
                (Seq.contains (Number (Natural (a + b))) e) -> Natural (a + b) |> Number            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) &&
                isLeastResidueSystem (Expressions e) -> 
                    let m = Seq.length e |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ -> (e1,op,e2,s) |> BinaryOp
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) && 
                (Seq.contains (Natural (a + b)) n) -> Natural (a + b) |> Number            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) &&
                isLeastResidueSystem (Numbers n) -> 
                    let m = Seq.length n |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ -> (e1,op,e2,s) |> BinaryOp
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

module IntegerNumbers = 
    let compare this that = 
        match this, that with
        | Integer x, Integer y when x > y -> GreaterThan |> Relation |> Symbol
        | Integer x, Integer y when x < y -> LessThan |> Relation |> Symbol
        | Integer x, Integer y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol     
    let highestCommonFactor x y = BigInteger.GreatestCommonDivisor (x, y)
    let abs x = Integer (abs x)        
    let isNegative this = compare this (Integer 0I) = (LessThan |> Relation |> Symbol)
        
module RationalNumbers =        
    let compare this that = 
        match this, that with
        | Rational x, Rational y ->
            match (x.numerator * y.denominator) > (x.denominator * y.numerator) with         
            | true -> GreaterThan |> Relation |> Symbol
            | false -> 
                match (x.numerator * y.denominator) = (x.denominator * y.numerator) with
                | true -> Equal |> Relation |> Symbol
                | false -> LessThan |> Relation |> Symbol
        | Integer x, Rational y when y.numerator > x * y.denominator -> LessThan |> Relation |> Symbol
        | Integer x, Rational y when y.numerator < x * y.denominator -> GreaterThan |> Relation |> Symbol
        | Integer x, Rational y when y.numerator = x * y.denominator -> Equal |> Relation |> Symbol
        | Rational x, Integer y when x.numerator > y * x.denominator -> GreaterThan |> Relation |> Symbol
        | Rational x, Integer y when x.numerator < y * x.denominator -> LessThan |> Relation |> Symbol
        | Rational x, Integer y when x.numerator = y * x.denominator -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = Rational {numerator = abs x.numerator; denominator = abs x.denominator}        
    let floor this = 
        match this.numerator > 0I with
        | true -> this.numerator / this.denominator |> Integer
        | false -> (this.numerator / this.denominator) - 1I |> Integer
    let ceiling this = 
        match this.numerator > 0I with
        | true -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) + 1I |> Integer
                    | false -> this.numerator / this.denominator |> Integer
        | false -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) |> Integer
                    | false -> (this.numerator / this.denominator) - 1I |> Integer
    let zero = {numerator = 0I; denominator = 1I} |> Rational
    let isNegative this = compare this zero = (LessThan |> Relation |> Symbol)    

module DecimalNumbers =
    let compare this that = 
        match this, that with
        | Decimal x, Decimal y when x > y -> GreaterThan |> Relation |> Symbol
        | Decimal x, Decimal y when x = y -> Equal |> Relation |> Symbol
        | Decimal x, Decimal y when x < y -> LessThan |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = Decimal (abs x)
    let floor x = Decimal (floor x)
    let ceiling x = Decimal (ceil x)
    let isNegative this = compare this (Decimal 0M) = (LessThan |> Relation |> Symbol)

module RealNumbers = 
    let compare this that = 
        match this, that with
        | Real x, Real y when x > y -> GreaterThan |> Relation |> Symbol
        | Real x, Real y when x = y -> Equal |> Relation |> Symbol
        | Real x, Real y when x < y -> LessThan |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = Real (abs x)
    let floor x = Real (floor x)
    let ceiling x = Real (ceil x)
    let isNegative this = compare this (Real 0.0) = (LessThan |> Relation |> Symbol)

module ComplexNumbers = 
    let abs x = Real (Complex.Abs x)
    let isNegative this = 
        match this with        
        | Complex x -> x.Real < 0.0
        | _ -> false

module Infinity =

    let compare this that = 
        match this, that with
        | Natural x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Natural x, Infinity Negative -> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Natural x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Natural x-> LessThan |> Relation |> Symbol
        
        | Integer x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Integer x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Integer x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Integer x-> LessThan |> Relation |> Symbol
        
        | Rational x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Rational x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Rational x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Rational x-> LessThan |> Relation |> Symbol
        
        | Decimal x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Decimal x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Decimal x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Decimal x-> LessThan |> Relation |> Symbol
        
        | Real x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Real x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Real x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Real x-> LessThan |> Relation |> Symbol

        | _ -> RelationUndefined |> Error |> Symbol

module Number =    
    let compare this that =
        match this, that with 
        | Natural x, Natural y -> (NaturalNumbers.compare this that) 
        | Integer x, Integer y -> (IntegerNumbers.compare this that) 
        | Integer x, Rational y -> (RationalNumbers.compare this that)
        | Rational x, Integer y -> (RationalNumbers.compare this that)
        | Rational x, Rational y -> (RationalNumbers.compare this that)
        | Decimal x, Decimal y -> (DecimalNumbers.compare this that)
        | Real x, Real y -> (RealNumbers.compare this that)        
        | Infinity i, _ -> (Infinity.compare this that)
        | _, Infinity i -> (Infinity.compare this that)        
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = 
        match x with
        | Integer x -> IntegerNumbers.abs x       
        | Rational x -> RationalNumbers.abs x
        | Decimal x -> DecimalNumbers.abs x
        | Real x -> RealNumbers.abs x
        | Complex x -> ComplexNumbers.abs x
        | _ -> Undefined
    let floor x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumbers.floor r
        | Decimal d -> DecimalNumbers.floor d
        | Real r -> RealNumbers.floor r       
        | _ -> Undefined
    let ceiling x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumbers.ceiling r
        | Decimal d -> DecimalNumbers.ceiling d
        | Real r -> RealNumbers.ceiling r
        | _ -> Undefined
    let min x y =
        match compare x y with
        | Symbol (Relation GreaterThan)  -> y
        | Symbol (Relation LessThan) -> x
        | _ -> x
    let max x y =
        match compare x y with
        | Symbol (Relation GreaterThan) -> x
        | Symbol (Relation LessThan) -> y
        | _ -> x
    let isNegative this = 
        match this with        
        | Rational x -> RationalNumbers.isNegative this
        | Integer x -> IntegerNumbers.isNegative this
        | Decimal d -> DecimalNumbers.isNegative this
        | Real x -> RealNumbers.isNegative this        
        | Complex x -> ComplexNumbers.isNegative this
        | _ -> false