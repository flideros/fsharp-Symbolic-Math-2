namespace MathObject

open System.Numerics
open Relations

module NaturalNumber =     
(*
The system of natural numbers is a set N = {0; 1; 2; 3; ...} together with three binary relations,
addition, multiplication, order relation and axioms governing their interaction.
*)    
    let set = N
    let axioms =
        [AssociativeAddition; CommutativeAddition; AdditiveIdentity; AdditiveCancellation;
         AssociativeMultiplication; CommutativeMultiplication; MultiplicativeIdentity; MultiplicativeCancellation;
         Distributive; ]
    let compare this that = 
        match this, that with
        | Natural x, Natural y when x > y -> GreaterThan |> Relation |> Symbol
        | Natural x, Natural y when x < y -> LessThan |> Relation |> Symbol
        | Natural x, Natural y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol

module IntegerNumber = 
    let compare this that = 
        match this, that with
        | Integer x, Integer y when x > y -> GreaterThan |> Relation |> Symbol
        | Integer x, Integer y when x < y -> LessThan |> Relation |> Symbol
        | Integer x, Integer y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol     
    let highestCommonFactor x y = BigInteger.GreatestCommonDivisor (x, y)
    let abs x = Integer (abs x)        
    let isNegative this = compare this (Integer 0I) = (LessThan |> Relation |> Symbol)
        
module RationalNumber =        
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

module DecimalNumber =
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

module RealNumber = 
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

module ComplexNumber = 
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
        | Natural x, Natural y -> (NaturalNumber.compare this that) 
        | Integer x, Integer y -> (IntegerNumber.compare this that) 
        | Integer x, Rational y -> (RationalNumber.compare this that)
        | Rational x, Integer y -> (RationalNumber.compare this that)
        | Rational x, Rational y -> (RationalNumber.compare this that)
        | Decimal x, Decimal y -> (DecimalNumber.compare this that)
        | Real x, Real y -> (RealNumber.compare this that)        
        | Infinity i, _ -> (Infinity.compare this that)
        | _, Infinity i -> (Infinity.compare this that)        
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = 
        match x with
        | Integer x -> IntegerNumber.abs x       
        | Rational x -> RationalNumber.abs x
        | Decimal x -> DecimalNumber.abs x
        | Real x -> RealNumber.abs x
        | Complex x -> ComplexNumber.abs x
        | _ -> Undefined
    let floor x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumber.floor r
        | Decimal d -> DecimalNumber.floor d
        | Real r -> RealNumber.floor r       
        | _ -> Undefined
    let ceiling x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumber.ceiling r
        | Decimal d -> DecimalNumber.ceiling d
        | Real r -> RealNumber.ceiling r
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
        | Rational x -> RationalNumber.isNegative this
        | Integer x -> IntegerNumber.isNegative this
        | Decimal d -> DecimalNumber.isNegative this
        | Real x -> RealNumber.isNegative this        
        | Complex x -> ComplexNumber.isNegative this
        | _ -> false