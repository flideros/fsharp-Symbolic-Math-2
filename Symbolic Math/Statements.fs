module Statements

type Axiom = 
// Properties    
    /// a + b is in the set of discourse
    | ClosureUnderAddition
    /// a x b is in the set of discourse
    | ClosureUnderMultiplication
    /// (a + b) + c = a + (b + c)
    | AssociativeAddition
    /// (a x b) x c = a x (b x c)
    | AssociativeMultiplication
    /// a + b = b + a
    | CommutativeAddition
    /// a x b = b x a
    | CommutativeMultiplication
    /// a + 0 = a
    | AdditiveIdentity          
    /// a x 1 = a
    | MultiplicativeIdentity
    /// a + -a = 0
    | AdditiveInverses
    /// For a not equal to 0, a x a^-1 = 1
    | MultiplicativeInverses
    /// a x (b + c) = (a x b) + (a x c)  
    | Distributive
    /// a + b = c + b , a = c
    | AdditiveCancellation
    /// a x b = c x b , a = c
    | MultiplicativeCancellation    
// Relation
    /// x < y iff there exists a z such that x + z = y;
    | Successor
    /// a can not be less than a
    | Irreflexivity
    /// Exactly one of (a < b, a = b,b < a) holds
    | Antisymmetry
    /// if a < b then not b < a
    | Asymmetric
    /// a < b and b < c then a < c;
    | Transitivity
    /// if a not = b then a < b or b < a
    | Connected    
    /// a < b implies a + c < b + c
    | AdditiveOrderPreserving
    /// a < b implies a x c < b x c
    | MultiplicativeOrderPreserving    
// Set
    /// Any subset has a least element.
    | WellOrdering
    /// There is an inductive set.
    | OfInfinity
    /// If a set S of numbers contains zero and also the 
    /// successor of every number in S, then every number is in S
    | Induction
    | EmptySet
    | Extensionality
    | Pairing
    | Union
    | PowerSet
    | SchemaOfSpecification