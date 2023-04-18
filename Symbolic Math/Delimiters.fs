module Delimiters

    type DelimiterType = {start:string;close:string}

    type AngleBrackets = DelimiterType 
    module AngleBrackets =
        let delimiter = {start="<";close=">"}
    
    type Bars = DelimiterType 
    module Bars =
        let delimiter = {start="|";close="|"}

    type Braces = DelimiterType
    module Braces =
        let delimiter = {start="{";close="}"}
    
    type Brackets = DelimiterType
    module Brackets =
        let delimiter = {start="[";close="]"}    

    type Parentheses = DelimiterType
    module Parentheses =
        let delimiter = {start="(";close=")"}