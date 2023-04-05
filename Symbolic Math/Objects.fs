namespace MathObject
// Data structures of the objects used to make math.
open System.Numerics
open Relations
(**)

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
    | RelationUndefined
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
    | Relation of Relation
    | Connectives //TODO
    | Quantifiers //TODO
    | Deliminators //TODO
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
    | Empty // The Empty Set
    | Numbers of seq<NumberType>
    | Expression of seq<Expression>

type UnaryOp = Set -> Operation -> Expression -> Expression
type BinaryOp = Set -> Expression -> Operation -> Expression -> Expression   
type NaryOp = Set -> Operation -> Expression list -> Expression

type Operations = 
    {addition:BinaryOp option
     subtraction:BinaryOp option
     multiplication:BinaryOp option
     division:BinaryOp option
     additiveInverse:UnaryOp option
     multiplicativeInverse:UnaryOp option}

type Axiom = 
    /// a + b is in the set of discourse
    | ClosureUnderAddition
    /// a x b is in the set of discourse
    | ClosureUnderMultiplication
    /// (a + b) + c = a + (b + c)
    | AssociativeAddition
    /// (a x b) x c = a x (b x c)
    | AssociativeMultiplication
    /// a + b = b + a
    | CommutativeAddition
    /// a x b = b x a
    | CommutativeMultiplication
    /// a + 0 = a
    | AdditiveIdentity          
    /// a x 1 = a
    | MultiplicativeIdentity
    /// a + -a = 0
    | AdditiveInverses
    /// For a not equal to 0, a x a^-1 = 1
    | MultiplicativeInverses
    /// a x (b + c) = (a x b) + (a x c)  
    | Distributive
    /// a + b = c + b , a = c
    | AdditiveCancellation
    /// a x b = c x b , a = c
    | MultiplicativeCancellation
    /// There is an inductive set.
    | Induction
    /// a ≮can not be less than a
    | Irreflexivity
    /// Exactly one of (a < b, a = b,b < a) holds
    | Antisymmetry
    /// x < y and y < z  x < z;
    | Transitivity
    /// Any subset has a least element.
    | WellOrdering
    /// a < b implies a + c < b + c
    | AdditiveOrderPreserving
    /// a < b implies a x c < b + c
    | MultiplicativeOrderPreserving

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
