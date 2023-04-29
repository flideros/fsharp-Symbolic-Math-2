module Constants 
(*  *)
    open System.Numerics

    type Pi = float
    module Pi = 
        let value = System.Math.PI
        let symbol = "\u03C0"   

    type EulerNumber = float
    module EulerNumber = 
        let value = System.Math.E
        let symbol = "\u0065" 

    type GoldenRatio = float
    module GoldenRatio =    
        let value = (1. + (System.Math.Sqrt 5))/2.
        let symbol = "\u03C6" 
    
    type Identity = string
    module Identity =
        let symbol = "\u2147"
        // Set_Operation = Identity
        let real_Addition = 0.0
        let real_Multiplication = 1.0
        let complex_Addition = Complex (0,0)
        let complex_Multiplication = Complex (1,0)
        let positiveIntegers_LCM = 1I
        let nonNegativeIntegers_GCD = 0I
        // TODO add more sets and operations
