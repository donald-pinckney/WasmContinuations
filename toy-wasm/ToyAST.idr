module ToyAST

import Data.Fin
import Data.Vect

%default covering

public export
data Value = ValueInt Int | ValueFloat Double | ValueBool Bool

public export
data Type' = TypeInt | TypeDouble | TypeBool

public export
data Expr : (cd : Nat) -> (fns : Nat) -> Type where
    ExprValue : Value -> Expr cd fns
    ExprVar : (var : Fin cd) -> Expr cd fns
    ExprDeclareVar : Type' -> (initExpr : Expr cd fns) -> (after : Expr (S cd) fns) -> Expr cd fns
    ExprUpdateVar : (var : Fin cd) -> (newExpr : Expr cd fns) -> (after : Expr cd fns) -> Expr cd fns
    ExprCall : (f : Fin fns) -> (args : List (Expr cd fns)) -> Expr cd fns
    ExprIf : (cond : Expr cd fns) -> Type' -> (true : Expr cd fns) -> (false : Expr cd fns) -> Expr cd fns
    ExprWhile : (cond : Expr cd fns) -> (body : Expr cd fns) -> (after : Expr cd fns) -> Expr cd fns
    ExprIAdd : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFAdd : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprISub : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFSub : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIMul : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFMul : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIDiv : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFDiv : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIMod : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIGT : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFGT : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIGTE : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFGTE : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprIEQ : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFEQ : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprILTE : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFLTE : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprILT : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprFLT : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprAnd : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprOr : Expr cd fns -> Expr cd fns -> Expr cd fns
    ExprNot : Expr cd fns -> Expr cd fns

public export
record FuncDef (fns : Nat) where
    constructor MkFuncDef
    returnType : Type'
    argumentTypes : List Type'
    body : Expr (length argumentTypes) fns

export
record Module (numNonMainFunctions : Nat) where
    constructor MkModule
    -- The first function is always the main function.
    -- The main function always has 0 parameters
    functions : Vect (S numNonMainFunctions) (FuncDef (S numNonMainFunctions))

export
typeOfValue : Value -> Type'
typeOfValue (ValueInt x) = TypeInt
typeOfValue (ValueBool x) = TypeBool
typeOfValue (ValueFloat x) = TypeDouble

public export
idrisTypeOfType : Type' -> Type
idrisTypeOfType TypeInt = Int
idrisTypeOfType TypeDouble = Double
idrisTypeOfType TypeBool = Bool

export
implementation Eq Type' where
    TypeDouble == TypeDouble = True
    TypeInt == TypeInt = True
    TypeBool == TypeBool = True
    _ == _ = False

export
implementation Eq Value where
    (ValueInt x) == (ValueInt y) = x == y
    (ValueFloat x) == (ValueFloat y) = x == y
    (ValueBool x) == (ValueBool y) = x == y
    _ == _ = False

export
implementation Show Type' where
    show TypeInt = "int"
    show TypeDouble = "float"
    show TypeBool = "bool"

export
implementation Show Value where
    show (ValueInt x) = show x
    show (ValueFloat x) = show x
    show (ValueBool x) = show x
