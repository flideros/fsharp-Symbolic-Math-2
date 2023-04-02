module Constants 
(*  *)
    type Pi = float
    module Pi = 
        let value = System.Math.PI
        let symbol = "\u03C0"   

    type EulerNumber = float
    module EulerNumber = 
        let value = System.Math.E
        let symbol = "\u0065"

    type Identity = string
    module Identity =
        let symbol = "\u2147"
        // Set_Operation = Identity
        let real_Addition = "0"
        let real_Multiplication = "1"
        let complex_Addition = "0"
        let complex_Multiplication = "1"
        let positiveIntegers_LCM = "1"
        let nonNegativeIntegers_GCD = "0"
        // TODO add more sets and operations
