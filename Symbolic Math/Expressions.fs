module Expressions
    
    open MathObject
    open System.Numerics
    open Statements
    open Relations
    open Operations
    open DefaultValues

    //Primitive Structure Operators
    let kind x = 
        match x with
        | Number (Integer i) -> "Integer"
        | Number (Rational r) -> "Rational"
        | Number (Real r) -> "Real"
        | BinaryOp(a,pow,Number(Real r),set) when pow = DefaultValues.pow -> "Real Power"        
        | _ -> "Undefined"
        // etc.
    let numberOfOperands x = 
        match x with
        | Number n -> Undefined        
        | Symbol v -> Undefined
        | BinaryOp (a,op,b,set) -> Integer 2I
        | UnaryOp (op,a,set) -> Integer 1I
        | NaryOp (op,aList,set) -> Integer (System.Numerics.BigInteger aList.Length)

    let operand a b =
        match a, b with
        | UnaryOp (op,u,set), 1 -> u
        | BinaryOp (x,op,y,set), 1 -> x
        | BinaryOp (x,op,y,set), 2 -> y
        | NaryOp (op,u,set), n when n > 0 && n <= u.Length -> u.[n-1]
        | _ -> Number Undefined
        
    let numberOfCompoundExpressions (x:Expression) = 
        let eNumber acc (n:Expression) = acc        
        let eSymbol acc (v:Expression) = acc
        let eBinaryOp acc x = acc + 1
        let eUnaryOp acc x = acc + 1
        let eNaryOp acc x = acc + 1
        let acc = 0
        Cata.foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp acc x

    let numberOfAtomicExpressions (x:Expression) = 
        let eNumber acc (n:Expression) = 1 + acc        
        let eSymbol acc (v:Expression) = 1 + acc
        let eBinaryOp acc x = acc
        let eUnaryOp acc x = acc
        let eNaryOp acc x = acc
        let acc = 0
        Cata.foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp acc x

    let subExpressions (x:Expression) = 
        let eNumber acc (n:Expression) = n::acc        
        let eSymbol acc (v:Expression) = v::acc
        let eBinaryOp acc x = x::acc
        let eUnaryOp acc x = x::acc
        let eNaryOp acc x = x::acc
        let acc = []
        Cata.foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp acc x

    let variables (x:Expression) = 
        let eNumber acc (n:Expression) = acc        
        let eSymbol acc (v:Expression) = match v with | Symbol (Variable v) -> Variable v::acc | Symbol (Constant c) -> acc | _ -> acc
        let eBinaryOp acc x = acc
        let eUnaryOp acc x = acc
        let eNaryOp acc x = acc
        let acc = []
        Cata.foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp acc x
        |> Seq.distinct
        |> Seq.toList

// Structure-based Operators  
    let freeOf u t =
        let completeSubExpressions = subExpressions u
        not (List.exists (fun x -> x = t) completeSubExpressions)

    let freeOfSort s = 
        let compare x y = 
            match freeOf x y with
            | true -> 1
            | false -> -1
        List.sortWith (fun x y -> compare x y) s
 
    let substitute (y, t) u =
        let eNumber (n:Expression) = (match n = y with | true -> t | false -> n)        
        let eSymbol (v:Expression) = (match v = y with | true -> t | false -> v)
        let eBinaryOp (a,op,b,set) = (match BinaryOp (a,op,b,set) = y with | true -> t | false -> BinaryOp (a,op,b,set))
        let eUnaryOp (op,a,set) = (match UnaryOp (op,a,set) = y with | true -> t | false -> UnaryOp (op,a,set))
        let eNaryOp (op,aList,set) = (match NaryOp (op,aList,set) = y with | true -> t | false -> NaryOp (op,aList,set))
        Cata.recurseExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp u
  
    let substituteSequential (yList : (Expression*Expression) list) u =        
        List.fold (fun u' (x,y) -> substitute (x, y) u') u yList

    let rec substituteConcurrent (yList : (Expression*Expression) list) = function
        | Number n ->
            let x = List.exists (fun x -> fst x = Number n) yList            
            match x with
            | true -> 
                let y = (List.find (fun x -> fst x = Number n) yList)
                (snd y)
            | false -> Number n        
        | Symbol v ->
            let x = List.exists (fun x -> fst x = Symbol v) yList
            match x with
            | true -> 
                let y = (List.find (fun x -> fst x = Symbol v) yList)
                (snd y)
            | false -> (Symbol v)
        | BinaryOp (a,op,b,set) ->
            let x = List.exists (fun x -> fst x = BinaryOp (a,op,b,set)) yList
            match x with
            | true -> 
                let y = (List.find (fun x -> fst x = BinaryOp (a,op,b,set)) yList)
                (snd y)
            | false -> (BinaryOp (substituteConcurrent yList a,op,substituteConcurrent yList b,set))
        | UnaryOp (op,a,set) ->
            let x = List.exists (fun x -> fst x = UnaryOp (op,a,set)) yList
            match x with
            | true -> 
                let y = (List.find (fun x -> fst x = UnaryOp (op,a,set)) yList)
                (snd y)
            | false -> (UnaryOp (op,substituteConcurrent yList a,set))
        | NaryOp (op,aList,set) ->
            let x = List.exists (fun x -> fst x = NaryOp (op,aList,set)) yList
            match x with
            | true -> 
                let y = (List.find (fun x -> fst x = NaryOp (op,aList,set)) yList)
                (snd y)
            | false -> (NaryOp (op,List.map (fun x -> substituteConcurrent yList x) aList,set))

    let rec numerator (u : Expression) = 
        match u with
        | Number (Rational {numerator = n; denominator = d}) -> (Number (Integer n)) //ND-1
        | BinaryOp (x, pow, Number (Integer n),set) when pow = DefaultValues.pow && n < 0I -> (Number (Integer 1I)) //ND-2
        | BinaryOp (x, pow, Number (Rational n),set) when pow = DefaultValues.pow && RationalNumbers.compare (Rational n) RationalNumbers.zero = Symbol (Relation LessThan) -> (Number (Integer 1I)) //ND-2
        | NaryOp(product,xList,set) when pow = DefaultValues.product -> //ND-3
            let v = operand u 1
            BinaryOp (numerator v, DefaultValues.times, numerator (BinaryOp (u, DefaultValues.divideBy, v,set)),set)
        | _ -> u //ND-4

    let rec denominator (u : Expression) = 
        match u with
        | Number (Rational {numerator = n; denominator = d}) -> (Number (Integer d)) //ND-1
        | BinaryOp (x, pow, Number (Integer n),set) when pow = DefaultValues.pow && n < 0I -> 
            BinaryOp (x, pow,UnaryOp (DefaultValues.addativeInverse,Number (Integer n),set),set) //ND-2 
        | BinaryOp (x, pow, Number (Rational r),set) when pow = DefaultValues.pow && RationalNumbers.compare (Rational r) RationalNumbers.zero = Symbol (Relation LessThan) -> 
            BinaryOp (x, pow,UnaryOp (DefaultValues.addativeInverse,Number (Rational r),set),set) //ND-2
        | NaryOp(product,xList,set) -> //ND-3
            let v = operand u 1
            BinaryOp (denominator v, DefaultValues.times, denominator (BinaryOp (u, DefaultValues.divideBy, v,set)),set)            
        | _ -> (Number (Integer 1I)) //ND-4
 
    //Comparison
    let rec compareExpressions u v =
        match u, v with
        | Number x, Number y -> 
            match Number.compare x y with //O-1
            | Symbol (Relation GreaterThan) -> 1
            | Symbol (Relation LessThan) -> -1
            | Symbol (Relation Equal) -> 0 
            | Symbol (Relation GreaterThanOrEqual) -> 1
            | Symbol (Relation LessThanOrEqual) -> -1
            | _ -> 0
        | Symbol x, Symbol y when x > y -> 1 //O-2
        | Symbol x, Symbol y when x < y -> -1 //O-2
        | Symbol x, Symbol y when x = y -> 0 //O-2
(*        | NaryOp(op1, x), NaryOp(op2, y) when //O-3.1 & O-6.2.(a)
            op1 = op2 && 
            (List.rev x).Head <> (List.rev y).Head ->
            compareExpressions ((List.rev x).Head) ((List.rev y).Head) 
        | NaryOp(op1, x), NaryOp(op2, y) when //O-3 & O-6.2
            op1 = op2 && 
            (List.rev x).Head = (List.rev y).Head ->
            match x.Tail.IsEmpty , y.Tail.IsEmpty with
            | false, false -> compareExpressions (NaryOp(op1, (List.rev((List.rev x).Tail)))) (NaryOp(op1, (List.rev((List.rev y).Tail)))) //O-3.2 & O-6.2.(b)
            | true, false -> 1 //O-3.3 & O-6.2.(c)
            | false, true -> -1 //O-3.3 & O-6.2.(c)
            | true, true -> 0        
       | BinaryOp(x1, op1, y1), BinaryOp(x2, op2, y2) when op1 = op2 && x1 <> x2 -> compareExpressions x1 x2 //O-4.1
        | BinaryOp(x1, op1, y1), BinaryOp(x2, op2, y2) when op1 = op2 && x1 = x2 -> compareExpressions y1 y2 //O-4.2        
        | UnaryOp(op1, x), UnaryOp(op2, y) when op1 = op2 -> compareExpressions x y //O-5
        | BinaryOp(x1, op1, y1), BinaryOp(x2, op2, y2) when op1 < op2 -> -1 //O-6.1
        | BinaryOp(x1, op1, y1), BinaryOp(x2, op2, y2) when op1 > op2 -> 1 //O-6.1
        | BinaryOp(x1, op1, y1), NaryOp(op2, y) when op1 < op2 -> -1 //O-6.1
        | BinaryOp(x1, op1, y1), NaryOp(op2, y) when op1 > op2 -> 1 //O-6.1
        | BinaryOp(x1, op1, y1), UnaryOp(op2, y) when op1 < op2 -> -1 //O-6.1
        | BinaryOp(x1, op1, y1), UnaryOp(op2, y) when op1 > op2 -> 1 //O-6.1
        | NaryOp(op1, x), NaryOp(op2, y) when op1 < op2 -> -1 //O-6.1
        | NaryOp(op1, x), NaryOp(op2, y) when op1 > op2 -> 1 //O-6.1
        | NaryOp(op1, x), BinaryOp(x2, op2, y2) when op1 < op2 -> -1 //O-6.1
        | NaryOp(op1, x), BinaryOp(x2, op2, y2) when op1 > op2 -> 1 //O-6.1
        | NaryOp(op1, x), UnaryOp(op2, y) when op1 < op2 -> -1 //O-6.1
        | NaryOp(op1, x), UnaryOp(op2, y) when op1 > op2 -> 1 //O-6.1        
        | UnaryOp(op1, x), UnaryOp(op2, y) when op1 > op2 -> -1 //O-6.1
        | UnaryOp(op1, x), UnaryOp(op2, y) when op1 < op2 -> 1 //O-6.1
        | UnaryOp(op1, x), BinaryOp(x2, op2, y2) when op1 > op2 -> -1 //O-6.1
        | UnaryOp(op1, x), BinaryOp(x2, op2, y2) when op1 < op2 -> 1 //O-6.1
        | UnaryOp(op1, x), NaryOp(op2, y) when op1 > op2 -> -1 //O-6.1
        | UnaryOp(op1, x), NaryOp(op2, y) when op1 < op2 -> 1 //O-6.1        
        | _, Number _ -> 1 //O-7
        | Number _, _ -> -1 //O-7 
        | NaryOp(Product, x), y -> compareExpressions (NaryOp(Product, x)) (NaryOp(Product, [y])) //O-8        
        | BinaryOp(base', ToThePowerOf, power'), b when base' <> Base b -> compareExpressions base' (Base b) //O-9
        | BinaryOp(base', ToThePowerOf, power'), b when base' = Base b -> compareExpressions power' (Exponent b) //O-9        
        | NaryOp(Sum, s), b -> compareExpressions (NaryOp(Sum, s)) (NaryOp(Sum, [b])) //O-10        
        | UnaryOp(Factorial, x), b when x = b -> -1 //O-11.1
        | UnaryOp(Factorial, x), b when x <> b -> compareExpressions (UnaryOp(Factorial, x)) (UnaryOp(Factorial, b)) //O-11.2        
        | NaryOp(op, x), Symbol v when x = [Symbol v] -> -1 //O-12.1
        | NaryOp(op, x), Symbol v when x <> [Symbol v] -> compareExpressions (NaryOp(op, x)) (NaryOp(op, [Symbol v])) //O-12.2
        | BinaryOp(x,op, y), Symbol v when x = Symbol v -> -1 //O-12.1
        | BinaryOp(x,op, y), Symbol v when x <> Symbol v -> compareExpressions (BinaryOp(x,op, y)) (BinaryOp(Symbol v,op, y)) //O-12.2
        | UnaryOp(op, x), Symbol v when x = Symbol v -> -1 //O-12.1
        | UnaryOp(op, x), Symbol v when x <> Symbol v -> compareExpressions (UnaryOp(op, x)) (UnaryOp(op, Symbol v)) //O-12.2        
        | _ -> -1 * (compareExpressions v u) //O-13
 
 *)