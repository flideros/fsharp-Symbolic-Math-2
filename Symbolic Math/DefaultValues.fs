module DefaultValues

open MathObject
open Operations

// Construct an operation.
let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
let sum = Addition (Addition.Sum (Sum.symbol, Sum.opPosition, Nary))
let product = Multiplication (Multiplication.Product (Product.symbol, Product.opPosition, Nary))
let minus = Subtraction (Subtraction.Minus (Minus.symbol, Minus.opPosition, Binary))
let times = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
let divideBy = Division (Division.DivideBy (Divide.symbol, Divide.opPosition, Binary))
let pow = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let invA = Exponentiation (Exponentiation.ToThePowerOf (ToThePowerOf.symbol, ToThePowerOf.opPosition, Binary))
let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
let root = Root (Root.SquareRootOf (SquareRootOf.symbol, SquareRootOf.opPosition, Unary))

// Get symbol string for a constant.
let piSymbol = Constants.Pi.symbol 
let eSymbol = Constants.EulerNumber.symbol