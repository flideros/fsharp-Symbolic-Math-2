﻿namespace MathObject

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
    let axioms =
        [AssociativeAddition; CommutativeAddition; AdditiveIdentity; AdditiveCancellation;
         AssociativeMultiplication; CommutativeMultiplication; MultiplicativeIdentity; MultiplicativeCancellation;
         Distributive; Induction; Successor; Transitivity; Antisymmetry]
    
    let leastResidueSystemSet modulus = seq { for i in 0UL .. modulus -> Natural i }
    let leastResidueSystemExpressionSet modulus = seq { for i in 0UL .. modulus -> Natural i |> Number}    
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
        | Numbers n -> 
            let m = Seq.length n - 1 |> uint64
            let comp a b = match compare a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (n,(leastResidueSystemSet m)) ||> Seq.compareWith comp = 0
        | Expressions e -> 
            let m = Seq.length e - 1 |> uint64
            let comp a b = match compareExpressions a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (e,(leastResidueSystemExpressionSet m)) ||> Seq.compareWith comp = 0
        | _ -> false

    let binaryAdd s e1 op e2 =
        match s, op with
        | N, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) -> Natural (a + b) |> Number
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                 
                (isLeastResidueSystem (Expressions e)) = false ->
                    let result = Natural (a + b) |> Number 
                    match (Seq.contains result e) &&
                          (Seq.contains e1 e) && 
                          (Seq.contains e2 e) with
                    | true -> result
                    | false -> NotInSet |> Error |> Symbol            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) &&
                isLeastResidueSystem (Expressions e) -> 
                    let m = Seq.length e |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                  
                isLeastResidueSystem (Numbers n) = false ->
                    let result = Natural (a + b)  
                    match (Seq.contains result n) &&
                          (Seq.contains (Natural a) n) && 
                          (Seq.contains (Natural b) n) with
                    | true -> result |> Number
                    | false -> NotInSet |> Error |> Symbol             
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) &&
                isLeastResidueSystem (Numbers n) -> 
                    let m = Seq.length n |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | N, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) -> Natural (a * b) |> Number
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                  
                isLeastResidueSystem (Expressions e) = false ->
                    let result = Natural (a * b) |> Number 
                    match (Seq.contains result e) &&
                          (Seq.contains e1 e) && 
                          (Seq.contains e2 e) with
                    | true -> result
                    | false -> NotInSet |> Error |> Symbol             
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) &&
                isLeastResidueSystem (Expressions e) -> 
                    let m = Seq.length e |> uint64
                    Natural ((a * b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when 
                isLeastResidueSystem (Numbers n) = false ->
                    let result = Natural (a * b)  
                    match (Seq.contains result n) &&
                          (Seq.contains (Natural a) n) && 
                          (Seq.contains (Natural b) n) with
                    | true -> result |> Number
                    | false -> NotInSet |> Error |> Symbol            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) &&
                isLeastResidueSystem (Numbers n) -> 
                    let m = Seq.length n |> uint64
                    Natural ((a * b) % m) |> Number            
            | Number (Natural a), _
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

    let operationServices =        
        {addition = Some binaryAdd
         subtraction = None
         multiplication = Some binaryMultiply
         division = None
         additiveInverse = None
         multiplicativeInverse = None
         toThePowerOf = None}

module IntegerNumbers = 
(*
Integers follow from the Natural numbers, but includes the negative numbers and Additive inverse.
*)    
    let set = Z      
    let axioms = AdditiveInverses::NaturalNumbers.axioms

    let compare this that = 
        match this, that with
        | Integer x, Integer y when x > y -> GreaterThan |> Relation |> Symbol
        | Integer x, Integer y when x < y -> LessThan |> Relation |> Symbol
        | Integer x, Integer y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol     
    let highestCommonFactor x y = BigInteger.GreatestCommonDivisor (x, y)
    let absoluteValue x =  (abs x) |> Integer  
    let isNegative this = compare this (Integer 0I) = (LessThan |> Relation |> Symbol)

    let binaryAdd s e1 op e2 =
        match s, op with
        | Z, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> Integer (a + b) |> Number
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) ->                  
                let result = Integer (a + b) |> Number 
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol                       
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a + b)  
                match (Seq.contains result n) &&
                      (Seq.contains (Integer a) n) &&
                      (Seq.contains (Integer b) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol          
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binarySubtract s e1 op e2 =
        match s, op with
        | Z, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> Integer (a - b) |> Number
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a - b) |> Number 
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol                       
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a - b)  
                match (Seq.contains result n) &&
                      (Seq.contains (Integer a) n) && 
                      (Seq.contains (Integer b) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | Z, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> Integer (a * b) |> Number
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a * b) |> Number 
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with                    
                | true -> result
                | false -> NotInSet |> Error |> Symbol            
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) ->                  
                let result = Integer (a * b)  
                match (Seq.contains result n) &&
                      (Seq.contains (Integer a) n) && 
                      (Seq.contains (Integer b) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryPower s e1 op e2 =        
        match s, op with
        | Z, Exponentiation (ToThePowerOf _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) when b >= 0I -> Integer (a ** int b) |> Number
            | Number (Integer a), Number (Integer b) when b < 0I -> 
                Rational {numerator = 1I * BigInteger a.Sign; denominator = ((abs a) ** int (abs b))} |> Number
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryAdditiveInverse s op e =
        match s, op with
        | Z, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Integer a) -> Integer -a  |> Number            
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Integer a) when (Seq.contains e ex) -> 
                let result = Integer -a |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol                                        
            | _ -> (op,e,s) |> UnaryOp            
        | Numbers n, Addition (Addition.Inverse _)  -> 
            match e with
            | Number (Integer a) when (Seq.contains (Integer a) n) -> 
                let result = Integer -a 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol             
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    
    let operationServices =        
        {addition = Some binaryAdd
         subtraction = Some binarySubtract
         multiplication = Some binaryMultiply
         division = None
         additiveInverse = Some unaryAdditiveInverse
         multiplicativeInverse = None
         toThePowerOf = Some binaryPower}

module RationalNumbers =        
(*
Rationals
*)    
    let set = Q
    let axioms = MultiplicativeInverses::IntegerNumbers.axioms
    
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

    let binaryAdd s e1 op e2 =
        match s, op with
        | Q, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                |> Number
            | Number (Rational r), Number (Integer i) //-> Rational r |> Number
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator + i * r.denominator
                let dTemp = r.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp } 
                |> Number
            | Number (Integer a), Number (Integer b) -> Integer (a + b) |> Number
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }                        
                match (Seq.contains result n) &&
                      (Seq.contains (Rational r1) n) && 
                      (Seq.contains (Rational r2) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol            
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binarySubtract s e1 op e2 =
        match s, op with
        | Q, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator - r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                |> Number
            | Number (Rational r), Number (Integer i) -> 
                let nTemp = r.numerator - i * r.denominator 
                let dTemp = r.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp } 
                |> Number
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = i * r.denominator - r.numerator
                let dTemp = r.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp } 
                |> Number
            | Number (Integer a), Number (Integer b) -> Integer (a + b) |> Number
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator - r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator - r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = IntegerNumbers.highestCommonFactor nTemp dTemp
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }                        
                match (Seq.contains result n) &&
                      (Seq.contains (Rational r1) n) && 
                      (Seq.contains (Rational r2) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol            
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

module DecimalNumbers =
(*
Decimals are represaented by the floating decimal point type variable. It uses 
128 bits for storing and representing data. Therefore, it has more precision 
(28-29 significant digits) than the float number type.
*)    
    let set = Q  

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
(*
Reals are represaented by the float number type which is a bianry point type. This means
that numbers are represented in the computer as binary numbers. Therefore, the accuracy is 
limited to about 15 significant digits
*)    
    let set = R  

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
(*
Complex
*)    
    let set = C  

    let abs x = Real (Complex.Abs x)
    let isNegative this = 
        match this with        
        | Complex x -> x.Real < 0.0
        | _ -> false

module Infinities =

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
        | Infinity i, _ -> (Infinities.compare this that)
        | _, Infinity i -> (Infinities.compare this that)        
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = 
        match x with
        | Integer x -> IntegerNumbers.absoluteValue x       
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