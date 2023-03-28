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
    type Addition = 
        | Plus of (string*OperatorPosition*Arity) 
        | Sum of (string*OperatorPosition*Arity) 

    module Times = 
        let symbol = "\u2A09"
        let opPosition = Infix
        let arity = Binary
    module Product = 
        let symbol = "\u220F"
        let opPosition = Prefix
        let arity = Nary
    type Multiplication = 
        | Times of (string*OperatorPosition*Arity) 
        | Product of (string*OperatorPosition*Arity) 


(*type Function = 
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
    | Sum           // n-ary op*)