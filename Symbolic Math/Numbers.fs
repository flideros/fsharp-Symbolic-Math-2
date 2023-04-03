namespace MathObject

open System.Numerics

(**)
// Assume that the natural numbers exist and directly give the rules of arithmetic and order relations for them.
module NaturalNumber = 
    
    let compare this that = 
        match this, that with
        | Natural x, Natural y when x > y -> 1
        | Natural x, Natural y when x < y -> -1
        | Natural x, Natural y when x = y -> 0
        | _ -> 2

module IntegerNumber = 
    let compare this that = 
        match this, that with
        | Integer x, Integer y when x > y -> 1
        | Integer x, Integer y when x < y -> -1
        | Integer x, Integer y when x = y -> 0
        | _ -> 2

module RationalNumber =
        
    let compare this that = 
        match this, that with
        | Rational x, Rational y ->
            match (x.numerator * y.denominator) > (x.denominator * y.numerator) with         
            | true -> 1
            | false -> 
                match (x.numerator * y.denominator) = (x.denominator * y.numerator) with
                | true -> 0
                | false -> -1
        | _ -> 2

    let floor this = 
        match this.numerator > 0I with
        | true -> this.numerator / this.denominator
        | false -> (this.numerator / this.denominator) - 1I 

    let ceiling this = 
        match this.numerator > 0I with
        | true -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) + 1I
                    | false -> this.numerator / this.denominator
        | false -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) 
                    | false -> (this.numerator / this.denominator) - 1I

    let zero = {numerator = 0I; denominator = 1I}

module DecimalNumber =
    let compare this that = 
        match this, that with
        | Decimal x, Decimal y when x > y -> 1
        | Decimal x, Decimal y when x = y -> 0
        | Decimal x, Decimal y when x < y -> -1
        | _ -> 2

module RealNumber = 
    let compare this that = 
        match this, that with
        | Real x, Real y when x > y -> 1
        | Real x, Real y when x = y -> 0
        | Real x, Real y when x < y -> -1
        | _ -> 2

module ComplexNumber = ()

module Infinity =

    let compare this that = 
        match this, that with
        | Natural x, Infinity Positive -> -1
        | Natural x, Infinity Negative -> 1
        | Infinity Positive, Natural x -> 1
        | Infinity Negative, Natural x-> -1
        
        | Integer x, Infinity Positive -> -1
        | Integer x, Infinity Negative-> 1
        | Infinity Positive, Integer x -> 1
        | Infinity Negative, Integer x-> -1
        
        | Rational x, Infinity Positive -> -1
        | Rational x, Infinity Negative-> 1
        | Infinity Positive, Rational x -> 1
        | Infinity Negative, Rational x-> -1
        
        | Decimal x, Infinity Positive -> -1
        | Decimal x, Infinity Negative-> 1
        | Infinity Positive, Decimal x -> 1
        | Infinity Negative, Decimal x-> -1
        
        | Real x, Infinity Positive -> -1
        | Real x, Infinity Negative-> 1
        | Infinity Positive, Real x -> 1
        | Infinity Negative, Real x-> -1

        | _ -> 2

module Number =

    module HCF = 
        let rec oF x y = BigInteger.GreatestCommonDivisor (x, y)
    
    let compare this that =
        match this, that with 
        | Natural x, Natural y -> (NaturalNumber.compare this that) 
        | Integer x, Integer y -> (IntegerNumber.compare this that) 
        | Rational x, Rational y -> (RationalNumber.compare this that)
        | Decimal x, Decimal y -> (DecimalNumber.compare this that)
        | Real x, Real y -> (RealNumber.compare this that)        
        
        | Integer x, Rational y when y.numerator > x * y.denominator -> -1
        | Integer x, Rational y when y.numerator < x * y.denominator -> 1
        | Integer x, Rational y when y.numerator = x * y.denominator -> 0
        | Rational x, Integer y when x.numerator > y * x.denominator -> 1
        | Rational x, Integer y when x.numerator < y * x.denominator -> -1
        | Rational x, Integer y when x.numerator = y * x.denominator -> 0        
        
        | _ -> 2

    let abs x = 
        match x with
        | Integer x -> Integer (abs x)        
        | Rational x -> Rational {numerator = abs x.numerator; denominator = abs x.denominator}
        | Decimal x -> Decimal (abs x)
        | Real x -> Real (abs x)
        | Complex x -> Real (Complex.Abs x)
        | _ -> Undefined

    let floor x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> Integer (RationalNumber.floor r)
        | Decimal d -> Decimal (floor d)
        | Real r -> Real (floor r)        
        | _ -> Undefined    

    let ceiling x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> Integer (RationalNumber.ceiling r)
        | Decimal d -> Decimal (ceil d)
        | Real r -> Real (ceil r)
        | _ -> Undefined      

    let min x y =
        match compare x y with
        | 1 -> y
        | -1 -> x
        | _ -> x

    let max x y =
        match compare x y with
        | 1 -> x
        | -1 -> y
        | _ -> x

    let isNegative this = 
        match this with        
        | Rational x when x.numerator * x.denominator < 0I -> true
        | Integer x when x < 0I -> true
        | Decimal d when d < 0M -> true
        | Real x when x < 0.0 -> true        
        | Complex x when x.Real < 0.0 -> true
        | _ -> false