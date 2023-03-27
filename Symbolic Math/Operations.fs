module Operations

    type operatorPosition =
        | Infix
        | Prefix

    type Addition = string
    module Plus = 
        let symbol = "\u002B"


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