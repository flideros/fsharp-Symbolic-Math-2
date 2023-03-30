module Operations

    type OperatorPosition =
        | Infix
        | Prefix
        | Postfix
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
        let symbol = "\u2A09"
        let symbolSmall = "\u00D7"
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

    type Addition = 
        | Plus of (string * OperatorPosition * Arity) 
        | Sum of (string * OperatorPosition * Arity) 
        | Inverse of (string * OperatorPosition * Arity)    
    type Multiplication = 
        | Times of (string * OperatorPosition * Arity) 
        | Product of (string * OperatorPosition * Arity)
        | Inverse of (string * OperatorPosition * Arity)
    type Subtraction = 
        | Minus of (string * OperatorPosition * Arity) 
    type Division = 
        | Divide of (string * OperatorPosition * Arity)
    



(*type Function = 
    | Lcm
    | Gcd    
    | ToThePowerOf  // binary op
    | Abs
    | Root
 *)