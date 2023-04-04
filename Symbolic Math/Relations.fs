module Relations

    type Relation =
        | Equal
        | GreaterThan
        | LessThan
        | GreaterThanOrEqual
        | LessThanOrEqual

    module Equal = 
        let symbol = "\u003D"
    module GreaterThan = 
        let symbol = "\u003E"
    module LessThan = 
        let symbol = "\u003C"
    module GreaterThanOrEqual = 
        let symbol = "\u2265"
    module LessThanOrEqual = 
        let symbol = "\u2264"