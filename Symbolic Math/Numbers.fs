namespace MathObject

open System.Numerics

module RationalNumber =
        
    let compareTo this that = 
        match (this.numerator*that.denominator) > (this.denominator*that.numerator) with
        | true -> 1
        | false -> match (this.numerator*that.denominator) = (this.denominator*that.numerator) with
                    | true -> 0
                    | false -> -1

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

module Number =

    module HCF = 
        let rec oF x y = BigInteger.GreatestCommonDivisor (x, y)
    
    let compare x y =
        match x,y with 
        | Natural x, Natural y when x > y -> 1
        | Natural x, Natural y when x = y -> 0
        | Natural x, Natural y when x < y -> -1    
        | Natural x, Infinity Positive -> -1
        | Natural x, Infinity Negative-> 1
        | Infinity Positive, Natural x -> 1
        | Infinity Negative, Natural x-> -1
        
        | Integer x, Integer y when x > y -> 1
        | Integer x, Integer y when x = y -> 0
        | Integer x, Integer y when x < y -> -1
        | Integer x, Infinity Positive -> -1
        | Integer x, Infinity Negative-> 1
        | Infinity Positive, Integer x -> 1
        | Infinity Negative, Integer x-> -1

        | Rational x, Rational y when y.numerator * x.denominator > x.numerator * y.denominator -> -1
        | Rational x, Rational y when y.numerator * x.denominator < x.numerator * y.denominator -> 1
        | Rational x, Rational y when y.numerator * x.denominator = x.numerator * y.denominator -> 0
        | Rational x, Infinity Positive -> -1
        | Rational x, Infinity Negative-> 1
        | Infinity Positive, Rational x -> 1
        | Infinity Negative, Rational x-> -1

        | Decimal x, Decimal y when x > y -> 1
        | Decimal x, Decimal y when x = y -> 0
        | Decimal x, Decimal y when x < y -> -1
        | Decimal x, Infinity Positive -> -1
        | Decimal x, Infinity Negative-> 1
        | Infinity Positive, Decimal x -> 1
        | Infinity Negative, Decimal x-> -1

        | Real x, Real y when x > y -> 1
        | Real x, Real y when x = y -> 0
        | Real x, Real y when x < y -> -1
        | Real x, Infinity Positive -> -1
        | Real x, Infinity Negative-> 1
        | Infinity Positive, Real x -> 1
        | Infinity Negative, Real x-> -1
        
        | Integer x, Rational y when y.numerator > x * y.denominator -> -1
        | Integer x, Rational y when y.numerator < x * y.denominator -> 1
        | Integer x, Rational y when y.numerator = x * y.denominator -> 0
        | Rational x, Integer y when x.numerator > y * x.denominator -> 1
        | Rational x, Integer y when x.numerator < y * x.denominator -> -1
        | Rational x, Integer y when x.numerator = y * x.denominator -> 0        
        | _ -> 0

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
        | Complex x when x.Real < 0.0 -> true
        | Rational x when x.numerator < 0I -> true
        | Integer x when x < 0I -> true
        | Real x when x < 0.0 -> true        
        | _ -> false