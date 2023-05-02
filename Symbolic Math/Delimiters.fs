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
    module SineFunction =
        let delimiter = {start="sin(";close=")"}
    module CosineFunction =
        let delimiter = {start="cos(";close=")"}
    module TangentFunction =
        let delimiter = {start="tan(";close=")"}
    module ArcineFunction =
        let delimiter = {start="asin(";close=")"}
    module ArcCosineFunction =
        let delimiter = {start="acos(";close=")"}
    module ArcTangentFunction =
        let delimiter = {start="atan(";close=")"}