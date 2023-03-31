namespace MathObject

open System.Numerics

(*
Data structures of the objects used to make math.
*)

type RationalNumber = {numerator: BigInteger; denominator: BigInteger}

type Infinity =
    | Negative
    | Positive

[<StructuralEquality;NoComparison>]
type NumberType =    
    | Natural of uint64
    | Integer of BigInteger 
    | Rational of RationalNumber
    | Decimal of decimal
    | Real of float
    | Complex of Complex
    | Infinity of Infinity
    | Undefined

type Error =
    | DivideByZero
    | Unknown

type Constant =
    | Pi of Constants.Pi
    | E of Constants.EulerNumber
    | Identity of Constants.Identity

type Operation =
    | Addition of Operations.Addition
    | Multiplication of Operations.Multiplication

type Symbol = 
    | Constant of Constant
    | Variable of string
    | Operation of Operation
    | Relation
    | Connectives
    | Quantifiers
    | Deliminators
    | Error of Error    
    | Inconsistent

type Expression =
    | Number of NumberType
    | Symbol of Symbol
    | BinaryOp of (Expression * Operation * Expression * Set)
    | UnaryOp of (Operation * Expression * Set) 
    | NaryOp of (Operation * (Expression list) * Set) 

and Set =  
    | N     // Set of all natural numbers
    | Z     // Set of all integers
    | Q     // Set of all rational numbers
    | R     // Set of all real numbers
    | C     // Set of all complex numbers
    | Zpos  // Set of all positive integers
    | Empty // The Empty Set
    | Numbers of seq<NumberType>
    | Expression of seq<Expression>

type UnaryOp = Set -> Operation -> Expression -> Expression
type BinaryOp = Set -> Expression -> Operation -> Expression -> Expression   

type Operations = 
    {addition:BinaryOp option
     subtraction:BinaryOp option
     multiplication:BinaryOp option
     division:BinaryOp option
     additiveInverse:UnaryOp option
     multiplicativeInverse:UnaryOp option}

type Axiom = 
    | AssociativeAddition       // (a + b) + c = a + (b + c)
    | AssociativeMultiplication // (a x b) x c = a x (b x c)
    | CommutativeAddition       // a + b = b + a
    | CommutativeMultiplication // a x b = b x a
    | AdditiveIdentity          // a + 0 = a
    | MultiplicativeIdentity    // a x 1 = a
    | AdditiveInverses          //a + -a = 0
    | MultiplicativeInverses    // For a not equal to 0, a x a^-1 = 1
    | Distributive              // a x (b + c) = (a x b) + (a x c)      

type AlgebraicStructure = Set * Operations * Axiom list

type MathematicalStructure =
    | Algebraic of AlgebraicStructure
    | Measure
    | Topology
    | Metric //Geometry
    | Order
    | Event
    | Equivalence
    | Differential
    | Category                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
