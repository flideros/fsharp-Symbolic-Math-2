﻿namespace MathObject
// Data structures of the objects used to make math.
open System.Numerics
open Statements
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
    | NotInSet
    | OperationUndefined
    | RelationUndefined
    | DivideByZero
    | Unknown

type Constant =
    | Pi of Constants.Pi
    | E of Constants.EulerNumber
    | GoldenRatio of Constants.GoldenRatio
    | Identity of Constants.Identity

type Operation =
    | Addition of Operations.Addition
    | Multiplication of Operations.Multiplication
    | Subtraction of Operations.Subtraction
    | Division of Operations.Division
    | Exponentiation of Operations.Exponentiation
    | Root of Operations.Root
    | AbsoluteValue of Operations.AbsoluteValue
    | TrigFunction of Operations.TrigonometricFunction

type Delimiter = 
    | AngleBrackets of Delimiters.AngleBrackets
    | Bars of Delimiters.Bars
    | Braces of Delimiters.Braces
    | Brackets of Delimiters.Brackets
    | Parentheses of Delimiters.Parentheses

type Symbol = 
    | Constant of Constant
    | Variable of string
    | Operation of Operation
    | Relation of Relation
    | Connectives //TODO
    | Quantifiers //TODO
    | Delimiters of Delimiter
    | Error of Error    
    | Inconsistent

type Expression =
    | Number of NumberType
    | Symbol of Symbol
    | BinaryOp of (Expression * Operation * Expression * Set)
    | UnaryOp of (Operation * Expression * Set) 
    | NaryOp of (Operation * (Expression list) * Set) 

and Set =  
    /// Set of all natural numbers
    | N     
    /// Set of all integers
    | Z     
    /// Set of all rational numbers
    | Q
    /// Set of all real numbers
    | R     
    /// Set of all complex numbers
    | C     
    /// The Empty Set
    | Empty 
    | Numbers of seq<NumberType>
    | Expressions of seq<Expression>

type UnaryOp = Set -> Operation -> Expression -> Expression
type BinaryOp = Set -> Expression -> Operation -> Expression -> Expression   
type NaryOp = Set -> Operation -> Expression list -> Expression

type Operations = 
    {addition:BinaryOp option
     subtraction:BinaryOp option
     multiplication:BinaryOp option
     division:BinaryOp option
     additiveInverse:UnaryOp option
     multiplicativeInverse:UnaryOp option
     toThePowerOf:BinaryOp option
     absoluteValue:UnaryOp option}

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
