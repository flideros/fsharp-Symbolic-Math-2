namespace MathObject

open System.Numerics

(*
Data structures of the objects used to make math.
*)

type RationalNumber = {numerator: BigInteger; denominator: BigInteger}

type NumberType =    
    | Natural of uint64
    | Integer of BigInteger 
    | Rational of RationalNumber
    | Decimal of decimal
    | Real of float
    | Complex of Complex
    | Undefined

type Error =
    | DivideByZero
    | Unknown

type Constant =
    | Pi of Constants.Pi
    | E of Constants.EulerNumber

type Symbol = 
    | Constant of Constant
    | Variable of string
    | Error of Error
    | Inconsistent

type Function = 
    | Lcm
    | Gcd
    | Plus          // binary op
    | UnaryMinus    // Unary op
    | Minus         // binary op
    | Times         // binary op
    | Product       // n-ary op
    | DividedBy     // binary op
    | ToThePowerOf  // binary op
    | Abs
    | Root
    | Sum           // n-ary op

type Expression =
    | Number of NumberType
    | Symbol of Symbol
    | BinaryOp of (Expression * Function * Expression * Set)
    | UnaryOp of (Function * Expression * Set) 
    | NaryOp of (Function * (Expression list) * Set) 

and Set =  
    | N     // Set of all natural numbers
    | Z     // Set of all integers
    | Q     // Set of all rational numbers
    | R     // Set of all real numbers
    | Zpos  // Set of all positive integers
    | Empty // The Empty Set
    | Numbers of seq<NumberType>
    | Expression of seq<Expression>

type AlgebraicStructure =
    | Ring
    | Field
    | Group
    | Lattice
    | Module

