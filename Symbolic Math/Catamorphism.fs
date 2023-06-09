module Cata  
    open MathObject
    ///bottom-up recursion
    let rec recurseExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp exp : 'r =
        let recurse = recurseExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp
        match exp with 
        | Number n -> eNumber (Number n)        
        | Symbol v -> eSymbol (Symbol v)
        | BinaryOp (a,op,b,s) -> eBinaryOp (recurse a,op,recurse b,s)
        | UnaryOp (op,a,s) -> eUnaryOp (op,recurse a,s)
        | NaryOp (op,aList,s) -> eNaryOp (op,(List.map recurse aList),s)

    ///top-down iteration
    let rec foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp acc exp : 'r =
        let recurse = foldExpression eNumber eSymbol eBinaryOp eUnaryOp eNaryOp
        match exp with 
        | Number n -> 
            let finalAcc = eNumber acc (Number n)
            finalAcc        
        | Symbol v -> 
            let finalAcc = eSymbol acc (Symbol v)
            finalAcc
        | BinaryOp (a,op,b,s) ->                        
            let newAcc = eBinaryOp acc (BinaryOp (a,op,b,s))
            [a;b] |> List.fold recurse newAcc
        | UnaryOp (op,a,s) -> 
            let newAcc = eUnaryOp acc (UnaryOp (op,a,s))
            recurse newAcc a
        | NaryOp (op,aList,s) -> 
            let newAcc = eNaryOp acc (NaryOp (op,aList,s))
            aList |> List.fold recurse newAcc  

    ///Bottom-up iteration
    let rec foldbackExpression eNumber eComplexNumber eSymbol eBinaryOp eUnaryOp eNaryOp generator exp :'r =
        let recurse = foldbackExpression eNumber eComplexNumber eSymbol eBinaryOp eUnaryOp eNaryOp 
        match exp with 
        | Number n -> generator (eNumber (Number n))                   
        | Symbol v -> generator (eSymbol (Symbol v))
        | BinaryOp (a,op,b,s) -> generator (eBinaryOp (recurse generator a,op,recurse generator b))
        | UnaryOp (op,a,s) ->  generator (eUnaryOp (op,recurse generator a))
        | NaryOp (op,aList,s) -> generator (eNaryOp(op,List.map (fun x -> recurse generator x) aList))
