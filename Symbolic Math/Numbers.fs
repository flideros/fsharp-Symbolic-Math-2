namespace MathObject

open System.Numerics
open Statements
open Relations
open Operations

module Patterns =
    open MathObject

    let rec (|RationalNumberExpression|_|) (u : Expression) =
        match u with
        | Number (Integer i) -> Some u //RNE-1 u is an integer
        | Number (Rational r) -> Some u //RNE-2 u is a fraction
        | BinaryOp (a,Exponentiation (Operations.ToThePowerOf _),(Number (Integer b)),_) when //RNE-7 u is a power with a base that is an RNE and an exponent that is an integer.
            (match a with | RationalNumberExpression a -> true | _ -> false) -> Some u 
        | BinaryOp (a,Addition (Operations.Plus _),b,_) when //RNE-3 u is a unary or binary sum with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) &&
            (match b with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Subtraction (Operations.Minus _),b,_) when //RNE-4 u is a unary or binary difference with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) &&
            (match b with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Multiplication (Operations.Times _),b,_) when //RNE-5 u is a binary product with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) &&
            (match b with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Division (Operations.DivideBy _),b,_) when //RNE-6 u is a quotient with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) &&
            (match b with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | UnaryOp (Addition (Operations.Addition.Inverse _),a,_) when //RNE-4 u is a unary or binary difference with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | UnaryOp (AbsoluteValue (Operations.AbsoluteValueOf _),a,_) when //RNE-3 u is a unary or binary sum with operands that are RNEs.
            (match a with | RationalNumberExpression a -> true | _ -> false) -> Some u
        | _-> None

    let rec (|GaussianRationalNumberExpression|_|) (u : Expression) =
        match u with
        | Number (Integer i) -> Some u //GRNE-1
        | Number (Rational r) -> Some u //GRNE-2
        | Symbol (Constant I) -> Some u //GRNE-3
        | BinaryOp (a,Exponentiation (Operations.ToThePowerOf _),(Number (Integer b)),_) when //GRNE-8
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u 
        | BinaryOp (a,Addition (Operations.Plus _),b,_) when //GRNE-4
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) &&
            (match b with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Subtraction (Operations.Minus _),b,_) when //GRNE-5
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) &&
            (match b with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Multiplication (Operations.Times _),b,_) when //GRNE-6
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) &&
            (match b with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | BinaryOp (a,Division (Operations.DivideBy _),b,_) when //GRNE-7
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) &&
            (match b with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | UnaryOp (Addition (Operations.Addition.Inverse _),a,_) when //GRNE-5
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | UnaryOp (AbsoluteValue (Operations.AbsoluteValueOf _),a,_) when //GRNE-4
            (match a with | GaussianRationalNumberExpression a -> true | _ -> false) -> Some u
        | _-> None
        
    let rec (|ExplicitAlgebraicNumber|_|) (u : Expression) =
        match u with
        | Number (Integer i) -> Some u //EAN-1
        | Number (Rational i) -> Some u //EAN-1
        | BinaryOp(ExplicitAlgebraicNumber a,Exponentiation (Operations.ToThePowerOf _),Number(Integer i),_) -> Some u //EAN-2
        | BinaryOp(ExplicitAlgebraicNumber a,Exponentiation (Operations.ToThePowerOf _),Number(Rational r),_) -> Some u //EAN-2
        | NaryOp(Multiplication (Operations.Product _),aList,_) 
            when List.forall (fun x -> match x with | ExplicitAlgebraicNumber a -> true | _ -> false) aList 
                -> Some u //EAN-3
        | NaryOp(Addition (Operations.Sum _),aList,_) 
            when List.forall (fun x -> match x with | ExplicitAlgebraicNumber a -> true | _ -> false) aList 
                -> Some u //EAN-3
        | _ -> None

module NaturalNumbers =     
(*
The system of natural numbers is a set N = {0; 1; 2; 3; ...} together with three binary relations,
addition, multiplication, order relation and axioms governing their interaction.
*)    
    let set = N
    let axioms =
        [AssociativeAddition; CommutativeAddition; AdditiveIdentity; AdditiveCancellation;
         AssociativeMultiplication; CommutativeMultiplication; MultiplicativeIdentity; MultiplicativeCancellation;
         Distributive; Induction; Successor; Transitivity; Antisymmetry]
    
    let leastResidueSystemSet modulus = seq { for i in 0UL .. modulus -> Natural i }
    let leastResidueSystemExpressionSet modulus = seq { for i in 0UL .. modulus -> Natural i |> Number}    
    let compare this that = 
        match this, that with
        | Natural x, Natural y when x > y -> GreaterThan |> Relation |> Symbol
        | Natural x, Natural y when x < y -> LessThan |> Relation |> Symbol
        | Natural x, Natural y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let compareExpressions this that = 
        match this, that with
        | Number (Natural x), Number (Natural y) when x > y -> GreaterThan |> Relation |> Symbol
        | Number (Natural x), Number (Natural y) when x < y -> LessThan |> Relation |> Symbol
        | Number (Natural x), Number (Natural y) when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let isLeastResidueSystem s = 
        match s with
        | N -> true
        | Numbers n -> 
            let m = Seq.length n - 1 |> uint64
            let comp a b = match compare a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (n,(leastResidueSystemSet m)) ||> Seq.compareWith comp = 0
        | Expressions e -> 
            let m = Seq.length e - 1 |> uint64
            let comp a b = match compareExpressions a b with | Symbol(Relation Equal) -> 0 | _ -> 1
            (e,(leastResidueSystemExpressionSet m)) ||> Seq.compareWith comp = 0
        | _ -> false

    let binaryAdd s e1 op e2 =
        match s, op with
        | N, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) -> Natural (a + b) |> Number
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                 
                (isLeastResidueSystem (Expressions e)) = false ->
                    let result = Natural (a + b) |> Number 
                    match (Seq.contains result e) &&
                          (Seq.contains e1 e) && 
                          (Seq.contains e2 e) with
                    | true -> result
                    | false -> NotInSet |> Error |> Symbol            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) &&
                isLeastResidueSystem (Expressions e) -> 
                    let m = Seq.length e |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                  
                isLeastResidueSystem (Numbers n) = false ->
                    let result = Natural (a + b)  
                    match (Seq.contains result n) &&
                          (Seq.contains (Natural a) n) && 
                          (Seq.contains (Natural b) n) with
                    | true -> result |> Number
                    | false -> NotInSet |> Error |> Symbol             
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) &&
                isLeastResidueSystem (Numbers n) -> 
                    let m = Seq.length n |> uint64
                    Natural ((a + b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | N, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) -> Natural (a * b) |> Number
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when                  
                isLeastResidueSystem (Expressions e) = false ->
                    let result = Natural (a * b) |> Number 
                    match (Seq.contains result e) &&
                          (Seq.contains e1 e) && 
                          (Seq.contains e2 e) with
                    | true -> result
                    | false -> NotInSet |> Error |> Symbol             
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains e1 e) && 
                (Seq.contains e2 e) &&
                isLeastResidueSystem (Expressions e) -> 
                    let m = Seq.length e |> uint64
                    Natural ((a * b) % m) |> Number            
            | Number (Natural a), _ 
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Natural a), Number (Natural b) when 
                isLeastResidueSystem (Numbers n) = false ->
                    let result = Natural (a * b)  
                    match (Seq.contains result n) &&
                          (Seq.contains (Natural a) n) && 
                          (Seq.contains (Natural b) n) with
                    | true -> result |> Number
                    | false -> NotInSet |> Error |> Symbol            
            | Number (Natural a), Number (Natural b) when 
                (Seq.contains (Natural a) n) && 
                (Seq.contains (Natural b) n) &&
                isLeastResidueSystem (Numbers n) -> 
                    let m = Seq.length n |> uint64
                    Natural ((a * b) % m) |> Number            
            | Number (Natural a), _
            | _, Number (Natural a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

    let operationServices =        
        {addition = Some binaryAdd
         subtraction = None
         multiplication = Some binaryMultiply
         division = None
         additiveInverse = None
         multiplicativeInverse = None
         toThePowerOf = None
         absoluteValue = None}

module IntegerNumbers = 
(*
Integers follow from the Natural numbers, but includes the negative numbers and Additive inverse.
*)    
    let set = Z      
    let axioms = AdditiveInverses::NaturalNumbers.axioms
    
    let compare this that = 
        match this, that with
        | Integer x, Integer y when x > y -> GreaterThan |> Relation |> Symbol
        | Integer x, Integer y when x < y -> LessThan |> Relation |> Symbol
        | Integer x, Integer y when x = y -> Equal |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol     
    /// Product of the smallest power of each common prime factor of two integers.
    let highestCommonFactor a b = 
        match a,b with
        | Integer x, Integer y -> BigInteger.GreatestCommonDivisor (x, y) |> Integer
        | _ -> Undefined
    /// Returns the truncated integer logarithm of a positive integer, base 10. 
    let log10 a =
        match a with
        | Integer i when i > 0I -> 
            let rec divisions acc num div =
                match num < div with
                | true -> acc, num
                | false -> divisions (acc + 1I) (num / div) div
            let c1, r1 = divisions 0I i (10I**1000)
            let c2, r2 = divisions 0I r1 (10I**100)
            let c3, r3 = divisions 0I r2 (10I**10)
            let c4, _r4 = divisions 0I r3 10I
            1000I * c1 + 100I * c2 + 10I * c3 + c4 |> Integer
        | _ -> Undefined
    /// Returns the truncated integer square root.
    let squareRoot a =
        let power10 a =
            match a with
            | Integer i when i > 0I -> 
                let rec expo product pow mult log10 =
                    match pow with
                    | x when x < log10 -> product, pow
                    | _ -> expo (product * mult) (pow - log10) mult log10
                let p1, r1 = expo 1I i  (10I**1000) 1000I
                let p2, r2 = expo 1I r1 (10I**100) 100I
                let p3, r3 = expo 1I r2 (10I**10) 10I
                let p4, _r4 = expo 1I r3 10I 1I
                p1 * p2 * p3 * p4
            | _ -> 0I
        let guess num =
            let log = log10 (Integer(num + 1I)) 
            let halfLog = 
                match log with 
                | Integer i when i > 0I -> (i + 1I) >>> 1
                | _ -> 0I
            (power10 (Integer halfLog))
        let rec converge oldGuess =
            let i = match a with | Integer i -> i | _ -> 0I
            let newGuess = (i / oldGuess + oldGuess) >>> 1
            match bigint.Abs (oldGuess - newGuess) with
            | x when x < 2I -> newGuess
            | _ -> converge newGuess    
        match a with 
        | Integer i when i > 0I ->
            let root = guess i |> converge 
            match root * root > i with
            | true -> root - 1I |> Integer
            | false -> root |> Integer
        | _ -> Undefined
    /// Returns the truncated integer quotient.
    let quotient a b =
        match a, b with 
        | (Integer x), (Integer y) when y <> 0I && x = 0I -> Integer 0I 
        | (Integer x), (Integer y) when y <> 0I ->
            let out = x/y
            let out' = 
                match (x >= y && x >= 0I) with
                | true -> out
                | false -> 
                    match y < 0I || ((bigint.Abs x) - (bigint.Abs (out * y))) = 0I with 
                    | true -> out + 1I
                    | false -> out - 1I
            match 0I <= (x - out' * y) && (x - out' * y) <= (bigint.Abs y) - 1I with
            | true -> Integer out'
            | false -> 
                match x < 0I with 
                | true -> Integer (out' - 1I)
                | false -> Integer (out' + 1I)
        | _ -> Undefined
    /// Returns the integer remainder of an integer quotient operation.
    let remainder a b =
        match a, b with
        | (Integer x), (Integer y) when y <> 0I -> 
            match quotient a b with
            | (Integer q) -> (Integer (x - q * y))
            | _ -> Undefined
        | _ -> Undefined

    let isNegative this = compare this (Integer 0I) = (LessThan |> Relation |> Symbol)
    let isPrimeNaive this =
        match this with
        | Integer n ->
            match n with
            | _ when n > 3I && (n % 2I = 0I || n % 3I = 0I) -> false
            | _ ->
                let maxDiv = System.Numerics.BigInteger(System.Math.Sqrt(float n)) + 1I
                let rec f d i = 
                    match d > maxDiv with 
                    | true -> true
                    | false -> 
                        match n % d = 0I with
                        | true -> false
                        | false -> f (d + i) (6I - i)     
                f 5I 2I
        | _ -> false
    let isPrime this =
        let cores = bigint (System.Environment.ProcessorCount * 16)
        match this with            
        | Integer n ->
            let maxDiv = (System.Numerics.BigInteger(System.Math.Sqrt(float n)) + 1I) / 3I
            let rec test (i : System.Numerics.BigInteger) max = 
                    let oe = match i.IsEven with | true -> 1I | false-> 2I
                    let oe' = match i.IsEven with | true -> 2I | false-> 1I
                    let out1,out2,out3 = n % (oe + (3I * i)),n % (oe' + (3I * (i + 1I))),n % (oe + (3I * (i+2I)))
                    match out1,out2,out3 with
                    | _ when i > max -> None
                    | _ when out1 = 0I || out2 = 0I || out3 = 0I -> Some false
                    | _ -> test (i + 3I) max                        
            let out = 
                match n with
                | _ when n = 5I || n = 7I || n = 11I -> None
                | _ when n > 3I && (n % 2I = 0I || n % 3I = 0I || n % 5I = 0I) -> Some false                
                | _ when n < 5000I -> test 1I maxDiv
                //| _ -> Async.Choice [async {return test 1I (maxDiv/2I)}; async {return test (maxDiv/2I) (maxDiv)}] |> Async.RunSynchronously 
                | _ -> Async.Choice [for i in 0I..(cores-1I) -> async {return test ((i*maxDiv/cores)+1I) ((i+1I)*maxDiv/cores)}] |> Async.RunSynchronously 
            match out with
            | Some false -> false
            | _ -> true                
        | _ -> false
    let isSquare this = 
        let thisValue = match this with | Integer i -> i | Undefined | _ -> 1I
        let sr = match squareRoot (this) with | Integer i -> i | Undefined | _ -> 0I
        // Return false or anything besides an integer square since 0*0 <> 1.
        sr*sr = thisValue

    let unaryAbsoluteValue s op e =
        match s, op with
        | Z, AbsoluteValue (AbsoluteValueOf _) -> 
            match e with            
            | Number (Integer a) when a >= 0I -> (Integer a) |> Number 
            | Number (Integer a) when a < 0I -> (Integer -a) |> Number
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryAdditiveInverse s op e =
        match s, op with
        | Z, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Integer a) -> Integer -a  |> Number            
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Integer a) when (Seq.contains e ex) -> 
                let result = Integer -a |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol                                        
            | _ -> (op,e,s) |> UnaryOp            
        | Numbers n, Addition (Addition.Inverse _)  -> 
            match e with
            | Number (Integer a) when (Seq.contains (Integer a) n) -> 
                let result = Integer -a 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol             
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryAdd s e1 op e2 =
        match s, op with
        | Z, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> Integer (a + b) |> Number
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) ->                  
                let result = Integer (a + b) |> Number 
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol                       
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a + b)  
                match (Seq.contains result n) &&
                      (Seq.contains (Integer a) n) &&
                      (Seq.contains (Integer b) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol          
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binarySubtract s e1 op e2 =
        let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
        let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
        match s, op with
        | Z, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus                     
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), _ 
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | Z, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> Integer (a * b) |> Number
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) -> 
                let result = Integer (a * b) |> Number 
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with                    
                | true -> result
                | false -> NotInSet |> Error |> Symbol            
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) ->                  
                let result = Integer (a * b)  
                match (Seq.contains result n) &&
                      (Seq.contains (Integer a) n) && 
                      (Seq.contains (Integer b) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryPower s e1 op e2 =        
        match s, op with
        | Z, Exponentiation (ToThePowerOf _) -> 
            match e1, e2 with
            | Number (Integer a), Number (Integer b) when b >= 0I -> Integer (a ** int b) |> Number
            | Number (Integer a), Number (Integer b) when b < 0I -> Rational {numerator = 1I * BigInteger a.Sign; denominator = ((abs a) ** int (abs b))} |> Number
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
        
    let operationServices =        
        {addition = Some binaryAdd
         subtraction = Some binarySubtract
         multiplication = Some binaryMultiply
         division = None
         additiveInverse = Some unaryAdditiveInverse
         multiplicativeInverse = None
         toThePowerOf = Some binaryPower
         absoluteValue = Some unaryAbsoluteValue}

    /// Sequence of prime integer numbers.
    let primes =
        let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary)) 
        let rec next x = seq{
            let test =
                match x with
                | Number (Integer x') when x' < 700I -> isPrimeNaive
                | _ -> isPrime
            let x' = match x with | Number n -> n | _ -> (Integer 0I)
            match test x' with
            | true when x = Number(Integer 2I) ->
                yield  (Integer 2I)
                yield! next (Number (Integer 3I))
            | true -> yield x' 
                      yield! next (binaryAdd Z x plus (Number(Integer 2I)))
            | false -> yield! next (binaryAdd Z x plus (Number(Integer 2I)))}
        next (Number (Integer 2I)) |> Seq.cache
    let primesUpTo max = 
        let e x = 
            match x with 
            | (Integer i) -> i 
            | _ -> 1I
        Seq.takeWhile (fun x -> e x < max) primes
    let primesUpToCount count = Seq.take count primes

module RationalNumbers =        
(*
Rationals
*)    
    let set = Q
    let axioms = MultiplicativeInverses::IntegerNumbers.axioms
    let zero = {numerator = 0I; denominator = 1I} |> Rational

    let compare this that = 
        match this, that with
        | Rational x, Rational y ->
            match (x.numerator * y.denominator) > (x.denominator * y.numerator) with         
            | true -> GreaterThan |> Relation |> Symbol
            | false -> 
                match (x.numerator * y.denominator) = (x.denominator * y.numerator) with
                | true -> Equal |> Relation |> Symbol
                | false -> LessThan |> Relation |> Symbol
        | Rational r, Integer i when r.numerator > i * r.denominator -> GreaterThan |> Relation |> Symbol
        | Integer i, Rational r when r.numerator < i * r.denominator -> GreaterThan |> Relation |> Symbol        
        | Integer i, Rational r when r.numerator > i * r.denominator -> LessThan |> Relation |> Symbol
        | Rational r, Integer i when r.numerator < i * r.denominator -> LessThan |> Relation |> Symbol
        | Integer i, Rational r when r.numerator = i * r.denominator -> Equal |> Relation |> Symbol
        | Rational r, Integer i when r.numerator = i * r.denominator -> Equal |> Relation |> Symbol
        | Integer x, Integer y -> IntegerNumbers.compare this that
        | _ -> RelationUndefined |> Error |> Symbol        
    let floor this = 
        match this.numerator > 0I with
        | true -> this.numerator / this.denominator |> Integer
        | false -> (this.numerator / this.denominator) - 1I |> Integer
    let ceiling this = 
        match this.numerator > 0I with
        | true -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) + 1I |> Integer
                    | false -> this.numerator / this.denominator |> Integer
        | false -> match snd (BigInteger.DivRem (this.numerator, this.denominator)) <> 0I with
                    | true -> (this.numerator / this.denominator) |> Integer
                    | false -> (this.numerator / this.denominator) - 1I |> Integer    
    
    let isNegative this = compare this zero = (LessThan |> Relation |> Symbol)    

    ///The absolute value function |*| : Q -> Q+ union zero
    let unaryAbsoluteValue s op e =
        match s, op with
        | Q, AbsoluteValue (AbsoluteValueOf _) -> 
            match e with
            | Number (Rational r) when compare (Rational r) zero = (GreaterThan |> Relation |> Symbol) || (Rational r) = zero -> 
                Rational r |> Number
            | Number (Rational r) when compare (Rational r) zero = (LessThan |> Relation |> Symbol) -> 
                Rational {numerator = abs r.numerator; denominator = abs r.denominator} |> Number
            | Number (Integer a) -> IntegerNumbers.unaryAbsoluteValue Z op e            
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryAdditiveInverse s op e =
        match s, op with
        | Q, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Rational r) -> Rational {r with numerator = -r.numerator} |> Number
            | Number (Integer a) -> Integer -a  |> Number            
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Rational r) when (Seq.contains e ex) -> 
                let result = Rational {r with numerator = -r.numerator} |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a) when (Seq.contains e ex) -> 
                let result = Integer -a |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Addition (Addition.Inverse _)  -> 
            match e with
            | Number (Rational r) when (Seq.contains (Rational r) n) -> 
                let result = Rational {r with numerator = -r.numerator} 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a) when (Seq.contains (Integer a) n) -> 
                let result = Integer -a 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryMultiplicativeInverse s op e =
        match s, op with
        | Q, Multiplication (Multiplication.Inverse _) -> 
            match e with            
            | Number (Rational r) when r.numerator = 0I -> DivideByZero |> Error |> Symbol            
            | Number (Rational r) when r.numerator = 1I -> Integer r.denominator |> Number
            | Number (Rational r) -> Rational {numerator = r.denominator; denominator = r.numerator } |> Number
            | Number (Integer a) when a = 0I -> DivideByZero |> Error |> Symbol
            | Number (Integer a) when a = 1I -> e
            | Number (Integer a) -> Rational {numerator = 1I; denominator = a}  |> Number
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Multiplication (Multiplication.Inverse _) -> 
            match e with
            | Number (Rational r) when (Seq.contains e ex) && r.numerator = 0I -> DivideByZero |> Error |> Symbol
            | Number (Rational r) when (Seq.contains e ex) && r.numerator = 1I && (Seq.contains (Integer r.denominator |> Number) ex) -> Integer r.denominator |> Number
            | Number (Rational r) when (Seq.contains e ex) && r.numerator <> 0I -> 
                let result = Rational {numerator = r.denominator; denominator = r.numerator} |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a) when a = 0I -> DivideByZero |> Error |> Symbol
            | Number (Integer a) when a = 1I && (Seq.contains e ex)-> e
            | Number (Integer a) when (Seq.contains e ex) -> 
                let result = Rational {numerator = 1I; denominator = a} |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Multiplication (Multiplication.Inverse _)  -> 
            match e with
            | Number (Rational r) when (Seq.contains (Rational r) n) && r.numerator = 0I -> DivideByZero |> Error |> Symbol
            | Number (Rational r) when (Seq.contains (Rational r) n) && r.numerator = 1I && (Seq.contains (Integer r.denominator ) n) -> Integer r.denominator |> Number
            | Number (Rational r) when (Seq.contains (Rational r) n) -> 
                let result = Rational {numerator = r.denominator; denominator = r.numerator} 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a) when a = 0I -> DivideByZero |> Error |> Symbol
            | Number (Integer a) when a = 1I && (Seq.contains (Integer a) n) -> e
            | Number (Integer a) when (Seq.contains (Integer a) n) -> 
                let result = Rational {numerator = 1I; denominator = a} 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol    
    let binaryAdd s e1 op e2 =
        match s, op with
        | Q, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator 
                let dTemp = r1.denominator * r2.denominator 
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                |> Number
            | Number (Rational r), Number (Integer i) 
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator + i * r.denominator
                let dTemp = r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp } 
                |> Number
            | Number (Integer a), Number (Integer b) -> Integer (a + b) |> Number
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Rational r), Number (Integer i) 
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator + i * r.denominator
                let dTemp = r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), Number (Integer b) -> 
                match (Seq.contains (Integer (a + b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Integer (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.denominator + r2.numerator * r1.denominator
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }                        
                match (Seq.contains result n) &&
                      (Seq.contains (Rational r1) n) && 
                      (Seq.contains (Rational r2) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol            
            | Number (Rational r), Number (Integer i) //-> Rational r |> Number
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator + i * r.denominator
                let dTemp = r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                match (Seq.contains result n) &&
                      (Seq.contains (Integer i) n) && 
                      (Seq.contains (Rational r) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), Number (Integer b) -> 
                match (Seq.contains (Integer (a + b)) n) &&
                      (Seq.contains (Integer a) n) && 
                      (Seq.contains (Integer b) n) with
                | true -> Integer (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol    
    let binarySubtract s e1 op e2 =
        let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
        let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
        match s, op with
        | Q, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus 
            | Number (Rational r), Number (Integer i) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer i), Number (Rational r) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Rational r), Number (Integer i) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus              
            | Number (Integer i), Number (Rational r) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus            
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Rational r), Number (Integer i) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer i), Number (Rational r) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), Number (Integer b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | Q, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.numerator 
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                |> Number
            | Number (Rational r), Number (Integer i)
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator * i * r.denominator
                let dTemp = r.denominator * r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                match dTemp / hcfTemp = 1I with
                | true -> Integer (nTemp / hcfTemp)
                | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp } 
                |> Number
            | Number (Integer a), Number (Integer b) -> Integer (a * b) |> Number
            | Number (Integer a), _
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.numerator 
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Rational r), Number (Integer i) 
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator * i * r.denominator
                let dTemp = r.denominator * r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                    |> Number
                match (Seq.contains result e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> result
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), Number (Integer b) -> 
                match (Seq.contains (Integer (a * b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Integer (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _
            | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ 
            | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> 
                let nTemp = r1.numerator * r2.numerator 
                let dTemp = r1.denominator * r2.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }                        
                match (Seq.contains result n) &&
                      (Seq.contains (Rational r1) n) && 
                      (Seq.contains (Rational r2) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol            
            | Number (Rational r), Number (Integer i) 
            | Number (Integer i), Number (Rational r) -> 
                let nTemp = r.numerator * i * r.denominator
                let dTemp = r.denominator * r.denominator
                let hcfTemp = match IntegerNumbers.highestCommonFactor (Integer nTemp) (Integer dTemp) with | Integer i -> i | _ -> 1I
                let result = 
                    match dTemp / hcfTemp = 1I with
                    | true -> Integer (nTemp / hcfTemp)
                    | false -> Rational { numerator = nTemp / hcfTemp; denominator = dTemp / hcfTemp }
                match (Seq.contains result n) &&
                      (Seq.contains (Rational r) n) && 
                      (Seq.contains (Integer i) n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), Number (Integer b) -> 
                match (Seq.contains (Integer (a * b)) n) &&
                      (Seq.contains (Integer a) n) && 
                      (Seq.contains (Integer b) n) with
                | true -> Integer (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryDivide s e1 op e2 =
        let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
        let multiply = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
        match s, op with
        | Q, Division (DivideBy _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply 
            | Number (Rational r), Number (Integer i) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer i), Number (Rational r) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer a), Number (Integer b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Division (DivideBy _) -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Rational r), Number (Integer i) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply              
            | Number (Integer i), Number (Rational r) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply           
            | Number (Integer a), Number (Integer b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Division (DivideBy _)  -> 
            match e1, e2 with
            | Number (Rational r1), Number (Rational r2) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Rational r), Number (Integer i) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer i), Number (Rational r) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer a), Number (Integer b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryPower s e1 op e2 =
        match s, op with
        | Q, Exponentiation (ToThePowerOf _) -> 
            match e1, e2 with
            | Number (Rational r), Number (Integer b) when b >= 0I -> Rational {numerator = (r.numerator ** int b) ; denominator = (r.denominator ** int b)} |> Number
            | Number (Rational r), Number (Integer b) when b < 0I -> Rational {numerator = (r.denominator ** int (abs b)) ; denominator = (r.numerator ** int (abs b))} |> Number
            | Number (Integer a), Number (Integer b) when b >= 0I -> Integer (a ** int b) |> Number
            | Number (Integer a), Number (Integer b) when b < 0I -> Rational {numerator = 1I * BigInteger a.Sign; denominator = ((abs a) ** int (abs b))} |> Number
            | Number (Integer a), _ | _, Number (Integer a) -> (e1,op,e2,s) |> BinaryOp
            | Number (Rational r), _ | _, Number (Rational r) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

    let operationServices =        
        {addition = Some binaryAdd
         subtraction = Some binarySubtract
         multiplication = Some binaryMultiply
         division = Some binaryDivide
         additiveInverse = Some unaryAdditiveInverse
         multiplicativeInverse = Some unaryMultiplicativeInverse
         toThePowerOf = Some binaryPower
         absoluteValue = Some unaryAbsoluteValue}

module DecimalNumbers =
(*
Decimals are represaented by the floating decimal point type variable. It uses 
128 bits for storing and representing data. Therefore, it has more precision 
(28-29 significant digits) than the float number type.
*)    
    let set = Q
    let axioms = MultiplicativeInverses::IntegerNumbers.axioms

    let compare this that = 
        match this, that with
        | Decimal x, Decimal y when x > y -> GreaterThan |> Relation |> Symbol
        | Decimal x, Decimal y when x = y -> Equal |> Relation |> Symbol
        | Decimal x, Decimal y when x < y -> LessThan |> Relation |> Symbol
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = Decimal (abs x)
    let floor x = Decimal (floor x)
    let ceiling x = Decimal (ceil x)
    let isNegative this = compare this (Decimal 0M) = (LessThan |> Relation |> Symbol)

    let unaryAbsoluteValue s op e =
        match s, op with
        | Q, AbsoluteValue (AbsoluteValueOf _) -> 
            match e with            
            | Number (Decimal a) when a < 0M -> Number (Decimal (-a))
            | Number (Decimal a) when a >= 0M -> Number (Decimal (a))
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryAdditiveInverse s op e =
        match s, op with
        | Q, Addition (Addition.Inverse _) -> 
            match e with            
            | Number (Decimal a) -> Decimal -a  |> Number            
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Decimal a) when (Seq.contains e ex) -> 
                let result = Decimal -a |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol            
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Addition (Addition.Inverse _)  -> 
            match e with            
            | Number (Decimal a) when (Seq.contains (Decimal a) n) -> 
                let result = Decimal -a 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryMultiplicativeInverse s op e =
        match s, op with
        | Q, Multiplication (Multiplication.Inverse _) -> 
            match e with            
            | Number (Decimal a) when a = 0M -> DivideByZero |> Error |> Symbol
            | Number (Decimal a) when a = 1M -> e
            | Number (Decimal a) -> Decimal (1M/a) |> Number
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Multiplication (Multiplication.Inverse _) -> 
            match e with
            | Number (Decimal a) when a = 0M -> DivideByZero |> Error |> Symbol
            | Number (Decimal a) when a = 1M && (Seq.contains e ex)-> e
            | Number (Decimal a) when (Seq.contains e ex) -> 
                let result = Decimal (1M/a) |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Multiplication (Multiplication.Inverse _)  -> 
            match e with
            | Number (Decimal a) when a = 0M -> DivideByZero |> Error |> Symbol
            | Number (Decimal a) when a = 1M && (Seq.contains (Decimal a) n) -> e
            | Number (Decimal a) when (Seq.contains (Decimal a) n) -> 
                let result = Decimal (1M/a)
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol    
    let binaryAdd s e1 op e2 =
        match s, op with
        | Q, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> Decimal (a + b) |> Number
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> 
                match (Seq.contains (Decimal (a + b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Decimal (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> 
                match (Seq.contains (Decimal (a + b)) n) &&
                      (Seq.contains (Decimal a) n) && 
                      (Seq.contains (Decimal b) n) with
                | true -> Decimal (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol    
    let binarySubtract s e1 op e2 =
        let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
        let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
        match s, op with
        | Q, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with            
            | Number (Decimal a), Number (Decimal b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | Q, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> Decimal (a * b) |> Number
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> 
                match (Seq.contains (Decimal (a * b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Decimal (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> 
                match (Seq.contains (Decimal (a * b)) n) &&
                      (Seq.contains (Decimal a) n) && 
                      (Seq.contains (Decimal b) n) with
                | true -> Decimal (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryDivide s e1 op e2 =
        let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
        let multiply = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
        match s, op with
        | Q, Division (DivideBy _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Division (DivideBy _) -> 
            match e1, e2 with          
            | Number (Decimal a), Number (Decimal b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Division (DivideBy _)  -> 
            match e1, e2 with
            | Number (Decimal a), Number (Decimal b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryPower s e1 op e2 =
        match s, op with
        | Q, Exponentiation (ToThePowerOf _) -> 
            match e1, e2 with
            | Number (Decimal a), Number (Integer b) when b > 0I -> seq { for i in 1 .. int b -> a } |> Seq.fold (fun acc x -> acc * x) 1M |> Decimal |> Number
            | Number (Decimal a), Number (Integer b) when b = 0I -> 1M |> Decimal |> Number
            | Number (Decimal a), Number (Integer b) when b < 0I -> 1M / (seq { for i in 1 .. int b -> a } |> Seq.fold (fun acc x -> acc * x) 1M) |> Decimal |> Number
            | Number (Decimal a), _ | _, Number (Decimal a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

    let operationServices =        
        {addition = Some binaryAdd
         subtraction = Some binarySubtract
         multiplication = Some binaryMultiply
         division = Some binaryDivide
         additiveInverse = Some unaryAdditiveInverse
         multiplicativeInverse = Some unaryMultiplicativeInverse
         toThePowerOf = Some binaryPower
         absoluteValue = Some unaryAbsoluteValue}

module RealNumbers =
(*
Reals are represented by the float number type which is a bianry point type. This means
that numbers are represented in the computer as binary numbers. Therefore, the accuracy is 
limited to about 15 significant digits.
*)    
    let set = R  
    let axioms = RationalNumbers.axioms

    let compare this that = 
        match this, that with
        | Real x, Real y when x > y -> GreaterThan |> Relation |> Symbol
        | Real x, Real y when x = y -> Equal |> Relation |> Symbol
        | Real x, Real y when x < y -> LessThan |> Relation |> Symbol
        
        | _ -> RelationUndefined |> Error |> Symbol
    let compareNumbers this that = 
        let getValue x = 
            match x with 
            | Number (Natural n) -> float n
            | Number (Integer i) -> float i
            | Number (Rational r) -> float r.numerator / float r.denominator
            | Number (Decimal d) -> float d
            | Number (Real r) -> r 
            | Symbol (Constant (Pi p)) -> Constants.Pi.value
            | Symbol (Constant (E e)) -> Constants.EulerNumber.value
            | Symbol (Constant (GoldenRatio e)) -> Constants.GoldenRatio.value
            | UnaryOp (Root(SquareRootOf _),Number (Integer i),s) 
                when i > 0I-> float i |> System.Math.Sqrt
            | _ -> nan
        match this, that with
        | x, y when (getValue x) > (getValue y) -> GreaterThan |> Relation |> Symbol
        | x, y when (getValue x) = (getValue y) -> Equal |> Relation |> Symbol
        | x, y when (getValue x) < (getValue y) -> LessThan |> Relation |> Symbol        
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = Real (abs x)
    let floor x = Real (floor x)
    let ceiling x = Real (ceil x)
    
    let isNegative this = compare this (Real 0.0) = (LessThan |> Relation |> Symbol)
    /// An irrational number cannot be expressed as a ratio of integers. 
    /// The decimal expansion of an irrational number is neither terminating nor recurring. 
    let rec isIrrational this =         
        match this with 
        | Number (Natural n) -> false
        | Number (Integer i) -> false
        | Number (Rational r) -> false
        | Number (Decimal d) -> false
        | Symbol (Constant (Pi pi)) when pi = Constants.Pi.value -> true
        | Symbol (Constant (E e)) when e = Constants.EulerNumber.value -> true        
        | UnaryOp (Root(SquareRootOf _),Number (Integer i),s) when IntegerNumbers.isSquare (Integer i) = false && i > 0I-> true
        | UnaryOp (Root(SquareRootOf _),Symbol (Constant (Pi pi)),s) -> true
        | UnaryOp (Root(SquareRootOf _),Symbol (Constant (E e)),s) -> true
        | UnaryOp (Root(SquareRootOf _),Symbol (Constant (GoldenRatio g)),s) -> true
        | BinaryOp (a, Addition (Plus _),b,s) 
        | BinaryOp (b, Addition (Plus _),a,s) when 
            isIrrational a = true &&
            isIrrational b = false -> true
        | BinaryOp (a, Multiplication (Times _),b,s) 
        | BinaryOp (b, Multiplication (Times _),a,s) when 
            isIrrational a = true &&
            isIrrational b = false -> true
        | _ -> false
    
    let getRealValue x = 
            match x with 
            | Number (Natural n) -> float n
            | Number (Integer i) -> float i
            | Number (Rational r) when r.denominator <> 0I -> float r.numerator / float r.denominator
            | Number (Decimal d) -> float d
            | Number (Real r) -> r 
            | Symbol (Constant (Pi p)) -> Constants.Pi.value
            | Symbol (Constant (E e)) -> Constants.EulerNumber.value
            | Symbol (Constant (GoldenRatio e)) -> Constants.GoldenRatio.value
            | UnaryOp (Root(SquareRootOf _),Number (Integer i),s)
                when i > 0I-> float i |> System.Math.Sqrt
            | BinaryOp (Number (Real r1), Addition (Plus _), Number (Real r2), s) -> r1 + r2
            | _ -> nan
    let evaluateExpression e =         
        let rec eval a =
            let tryValue = getRealValue a
            match a with
            | Number n when tryValue.ToString() <> "NaN" -> tryValue |> Real |> Number
            | Symbol (Constant c) when tryValue.ToString() <> "NaN" -> tryValue |> Real |> Number
            | UnaryOp (Root(SquareRootOf x),exp,s) -> 
                let result = (UnaryOp (Root(SquareRootOf x),eval exp,s)) |> getRealValue
                match result.ToString() = "NaN" with 
                | true -> a
                | false -> result |> Real |> Number
            | BinaryOp (e1, Addition (Plus p), e2, s) -> 
                let result = (BinaryOp (eval e1, Addition (Plus p), eval e2, s)) |> getRealValue 
                match result.ToString() = "NaN" with 
                | true -> a
                | false -> result |> Real |> Number
            | _ -> e
        eval e

(*      
    let simplifyRealExpression x = 
        let rec simplify a' =
            match a' with 
            
            | NaryOp(Sum,a) -> simplifyRealSum (NaryOp(Sum,(List.map simplify a)))            
            | NaryOp(Product,a) -> simplifyRealProduct (NaryOp(Product,(List.map simplify a)))
            | BinaryOp(a,ToThePowerOf,b) -> simplifyRealPower (BinaryOp(simplify a,ToThePowerOf,simplify b))
            | UnaryOp(Factorial,a) -> simplifyFactorial (simplify a) 
        simplify x
*)
    let unaryAbsoluteValue s op e =
        match s, op with
        | R, AbsoluteValue (AbsoluteValueOf _) -> 
            match e with            
            | Number (Real a) when a < 0. -> Number (Real (-a))
            | Number (Real a) when a >= 0. -> Number (Real (a))
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryAdditiveInverse s op e =
        match s, op with
        | R, Addition (Addition.Inverse _) -> 
            match e with            
            | Number (Real a) -> Real -a  |> Number            
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Addition (Addition.Inverse _) -> 
            match e with
            | Number (Real a) when (Seq.contains e ex) -> 
                let result = Real -a |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol            
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Addition (Addition.Inverse _)  -> 
            match e with            
            | Number (Real a) when (Seq.contains (Real a) n) -> 
                let result = Real -a 
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol
    let unaryMultiplicativeInverse s op e =
        match s, op with
        | R, Multiplication (Multiplication.Inverse _) -> 
            match e with            
            | Number (Real a) when a = 0. -> DivideByZero |> Error |> Symbol
            | Number (Real a) when a = 1. -> e
            | Number (Real a) -> Real (1./a) |> Number
            | _ -> (op,e,s) |> UnaryOp
        | Expressions ex, Multiplication (Multiplication.Inverse _) -> 
            match e with
            | Number (Real a) when a = 0. -> DivideByZero |> Error |> Symbol
            | Number (Real a) when a = 1. && (Seq.contains e ex)-> e
            | Number (Real a) when (Seq.contains e ex) -> 
                let result = Real (1./a) |> Number
                match (Seq.contains result ex) with
                | true -> result 
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | Numbers n, Multiplication (Multiplication.Inverse _)  -> 
            match e with
            | Number (Real a) when a = 0. -> DivideByZero |> Error |> Symbol
            | Number (Real a) when a = 1. && (Seq.contains (Real a) n) -> e
            | Number (Real a) when (Seq.contains (Real a) n) -> 
                let result = Real (1./a)
                match (Seq.contains result n) with
                | true -> result |> Number
                | false -> NotInSet |> Error |> Symbol
            | _ -> (op,e,s) |> UnaryOp
        | _ -> OperationUndefined |> Error |> Symbol    
    let binaryAdd s e1 op e2 =
        match s, op with
        | R, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> Real (a + b) |> Number
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Addition (Plus _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> 
                match (Seq.contains (Real (a + b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Real (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Addition (Plus _)  -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> 
                match (Seq.contains (Real (a + b)) n) &&
                      (Seq.contains (Real a) n) && 
                      (Seq.contains (Real b) n) with
                | true -> Real (a + b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol    
    let binarySubtract s e1 op e2 =
        let addativeInverse = Addition (Addition.Inverse (AddativeInverse.symbol, AddativeInverse.opPosition, Unary))
        let plus = Addition (Addition.Plus (Plus.symbol, Plus.opPosition, Binary))
        match s, op with
        | R, Subtraction (Minus _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Subtraction (Minus _) -> 
            match e1, e2 with            
            | Number (Real a), Number (Real b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Subtraction (Minus _)  -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> (unaryAdditiveInverse s addativeInverse e2) |> binaryAdd s e1 plus
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryMultiply s e1 op e2 =
        match s, op with
        | R, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> Real (a * b) |> Number
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Multiplication (Times _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> 
                match (Seq.contains (Real (a * b) |> Number) e) &&
                      (Seq.contains e1 e) && 
                      (Seq.contains e2 e) with
                | true -> Real (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Multiplication (Times _)  -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> 
                match (Seq.contains (Real (a * b)) n) &&
                      (Seq.contains (Real a) n) && 
                      (Seq.contains (Real b) n) with
                | true -> Real (a * b) |> Number
                | false -> NotInSet |> Error |> Symbol
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryDivide s e1 op e2 =
        let multiplicativeInverse = Multiplication (Multiplication.Inverse (MultiplicativeInverse.symbol, MultiplicativeInverse.opPosition, Unary))
        let multiply = Multiplication (Multiplication.Times (Times.symbol, Times.opPosition, Binary))
        match s, op with
        | R, Division (DivideBy _) -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | Expressions e, Division (DivideBy _) -> 
            match e1, e2 with          
            | Number (Real a), Number (Real b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp
            | _ -> OperationUndefined |> Error |> Symbol
        | Numbers n, Division (DivideBy _)  -> 
            match e1, e2 with
            | Number (Real a), Number (Real b) -> (unaryMultiplicativeInverse s multiplicativeInverse e2) |> binaryMultiply s e1 multiply
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol
    let binaryPower s e1 op e2 =
        match s, op with
        | R, Exponentiation (ToThePowerOf _) -> 
            match e1, e2 with
            | Number (Real a), Number (Integer b) when b > 0I -> seq { for i in 1 .. int b -> a } |> Seq.fold (fun acc x -> acc * x) 1. |> Real |> Number
            | Number (Real a), Number (Integer b) when b = 0I -> 1. |> Real |> Number
            | Number (Real a), Number (Integer b) when b < 0I -> 1. / (seq { for i in 1 .. int b -> a } |> Seq.fold (fun acc x -> acc * x) 1.) |> Real |> Number
            | Number (Real a), _ | _, Number (Real a) -> (e1,op,e2,s) |> BinaryOp            
            | _ -> OperationUndefined |> Error |> Symbol
        | _ -> OperationUndefined |> Error |> Symbol

    let operationServices =        
        {addition = Some binaryAdd
         subtraction = Some binarySubtract
         multiplication = Some binaryMultiply
         division = Some binaryDivide
         additiveInverse = Some unaryAdditiveInverse
         multiplicativeInverse = Some unaryMultiplicativeInverse
         toThePowerOf = Some binaryPower
         absoluteValue = Some unaryAbsoluteValue}

module ComplexNumbers = 
(*
Complex
*)    
    let set = C  

    let abs x = Real (Complex.Abs x)
    let isNegative this = 
        match this with        
        | Complex x -> x.Real < 0.0
        | _ -> false

module Infinities =

    let compare this that = 
        match this, that with
        | Natural x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Natural x, Infinity Negative -> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Natural x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Natural x-> LessThan |> Relation |> Symbol
        
        | Integer x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Integer x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Integer x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Integer x-> LessThan |> Relation |> Symbol
        
        | Rational x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Rational x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Rational x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Rational x-> LessThan |> Relation |> Symbol
        
        | Decimal x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Decimal x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Decimal x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Decimal x-> LessThan |> Relation |> Symbol
        
        | Real x, Infinity Positive -> LessThan |> Relation |> Symbol
        | Real x, Infinity Negative-> GreaterThan |> Relation |> Symbol
        | Infinity Positive, Real x -> GreaterThan |> Relation |> Symbol
        | Infinity Negative, Real x-> LessThan |> Relation |> Symbol

        | _ -> RelationUndefined |> Error |> Symbol

module Number =    
    let compare this that =
        match this, that with 
        | Natural x, Natural y -> (NaturalNumbers.compare this that) 
        | Integer x, Integer y -> (IntegerNumbers.compare this that) 
        | Integer x, Rational y -> (RationalNumbers.compare this that)
        | Rational x, Integer y -> (RationalNumbers.compare this that)
        | Rational x, Rational y -> (RationalNumbers.compare this that)
        | Decimal x, Decimal y -> (DecimalNumbers.compare this that)
        | Real x, Real y -> (RealNumbers.compare this that)        
        | Infinity i, _ -> (Infinities.compare this that)
        | _, Infinity i -> (Infinities.compare this that)        
        | _ -> RelationUndefined |> Error |> Symbol
    let abs x = 
        let absOf = AbsoluteValue (AbsoluteValueOf (Operations.AbsoluteValue.delimiter,Operations.AbsoluteValue.opPosition,Operations.AbsoluteValue.arity))
        match x with
        | Integer i -> IntegerNumbers.unaryAbsoluteValue Z absOf (Number (Integer i))       
        | Rational r -> IntegerNumbers.unaryAbsoluteValue Q absOf (Number (Rational r))
        | Decimal x -> DecimalNumbers.abs x |> Number
        | Real x -> RealNumbers.abs x |> Number
        | Complex x -> ComplexNumbers.abs x |> Number
        | _ -> Undefined |> Number
    let floor x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumbers.floor r
        | Decimal d -> DecimalNumbers.floor d
        | Real r -> RealNumbers.floor r       
        | _ -> Undefined
    let ceiling x = 
        match x with
        | Natural n -> x
        | Integer i -> x
        | Rational r -> RationalNumbers.ceiling r
        | Decimal d -> DecimalNumbers.ceiling d
        | Real r -> RealNumbers.ceiling r
        | _ -> Undefined
    let min x y =
        match compare x y with
        | Symbol (Relation GreaterThan)  -> y
        | Symbol (Relation LessThan) -> x
        | _ -> x
    let max x y =
        match compare x y with
        | Symbol (Relation GreaterThan) -> x
        | Symbol (Relation LessThan) -> y
        | _ -> x
    let isNegative this = 
        match this with        
        | Rational x -> RationalNumbers.isNegative this
        | Integer x -> IntegerNumbers.isNegative this
        | Decimal d -> DecimalNumbers.isNegative this
        | Real x -> RealNumbers.isNegative this        
        | Complex x -> ComplexNumbers.isNegative this
        | _ -> false