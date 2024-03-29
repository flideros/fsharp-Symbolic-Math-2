﻿module Operations
    open Delimiters
    type OperatorPosition =
        | Infix
        | Prefix
        | Postfix
        | TopPostfix
        | Bracket
    type Arity = 
        | Unary
        | Binary
        | Nary 
    
    module Plus = 
        let symbol = "\u002B"
        let opPosition = Infix
        let arity = Binary        
    module Sum = 
        let symbol = "\u2211"
        let opPosition = Prefix
        let arity = Nary        
    module AddativeInverse =
        let symbol = "\u2212"
        let opPosition = Prefix
        let arity = Unary         
    module Times =         
        let symbol = "\u00D7"
        let dotOperator = "\u22C5"
        let opPosition = Infix
        let arity = Binary        
    module Product = 
        let symbol = "\u220F"
        let opPosition = Prefix
        let arity = Nary 
    module MultiplicativeInverse =
        let symbol = "\u207B" + "\u00B9"
        let opPosition = Postfix
        let arity = Unary
    module Minus = 
        let symbol = "\u2212"
        let opPosition = Infix
        let arity = Binary
    module Divide = 
        let symbol = "\u2215"
        let opPosition = Infix
        let arity = Binary
    module ToThePowerOf =
        let symbol = "Pow"
        let opPosition = TopPostfix
        let arity = Binary
    module SquareRootOf =
        let symbol = "\u221A"
        let opPosition = Prefix
        let arity = Unary
    module AbsoluteValue =
        let symbol = "||"
        let delimiter = Bars.delimiter
        let opPosition = Bracket
        let arity = Unary
    module Factorial =
        let symbol = "!"        
        let opPosition = Postfix
        let arity = Unary

    type Addition = 
        | Plus of (string * OperatorPosition * Arity) 
        | Sum of (string * OperatorPosition * Arity) 
        | Inverse of (string * OperatorPosition * Arity)    
    type Multiplication = 
        | Times of (string * OperatorPosition * Arity) 
        | Product of (string * OperatorPosition * Arity)
        | Inverse of (string * OperatorPosition * Arity)
        | Factorial of (string * OperatorPosition * Arity)
    type Subtraction = 
        | Minus of (string * OperatorPosition * Arity) 
    type Division = 
        | DivideBy of (string * OperatorPosition * Arity)
    type Exponentiation =
        | ToThePowerOf of (string * OperatorPosition * Arity)
    type Root =
        | SquareRootOf of (string * OperatorPosition * Arity)
    type AbsoluteValue  = 
        | AbsoluteValueOf of (DelimiterType * OperatorPosition * Arity)
    
(* Trig functions *)
    module Sine =
        let symbol = "sin"
        let delimiter = SineFunction.delimiter
        let opPosition = Bracket
        let arity = Unary
    module Cosine =
        let symbol = "cos"
        let delimiter = CosineFunction.delimiter
        let opPosition = Bracket
        let arity = Unary
    module Tangent =
        let symbol = "tan"
        let delimiter = TangentFunction.delimiter
        let opPosition = Bracket
        let arity = Unary
    module ArcSine =
        let symbol = "asin"
        let delimiter = SineFunction.delimiter
        let opPosition = Bracket
        let arity = Unary
    module ArcCosine =
        let symbol = "acos"
        let delimiter = CosineFunction.delimiter
        let opPosition = Bracket
        let arity = Unary
    module ArcTangent =
        let symbol = "atan"
        let delimiter = TangentFunction.delimiter
        let opPosition = Bracket
        let arity = Unary

    type TrigonometricFunction  = 
        | Sine of (DelimiterType * OperatorPosition * Arity)
        | Cosine of (DelimiterType * OperatorPosition * Arity)
        | Tangent of (DelimiterType * OperatorPosition * Arity)
        | ArcSine of (DelimiterType * OperatorPosition * Arity)
        | ArcCosine of (DelimiterType * OperatorPosition * Arity)
        | ArcTangent of (DelimiterType * OperatorPosition * Arity)