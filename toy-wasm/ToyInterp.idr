module ToyInterp

import ToyAST
import Data.Vect
import Data.Fin

public export
Result : Type -> Type
Result t = Either String t

public export
State : Nat -> Type
State cd = Vect cd Value

public export
bool_to_int : Bool -> Int
bool_to_int False = 0
bool_to_int True = 1

public export
int_to_bool : Int -> Bool
int_to_bool x = not (x == 0)

public export
int_and : Int -> Int -> Int
int_and x y = bool_to_int (int_to_bool x && int_to_bool y)

public export
int_or : Int -> Int -> Int
int_or x y = bool_to_int (int_to_bool x || int_to_bool y)

public export
int_not : Int -> Int
int_not x = bool_to_int (not (int_to_bool x))

public export
comp2 : (c -> d) -> (a -> b -> c) -> (a -> b -> d)
comp2 f g x y = f (g x y)

public export
lookup_state : State cd -> Fin cd -> Value
lookup_state s i = index i s

public export
update_state : State cd -> Fin cd -> Value -> State cd
update_state s i x = replaceAt i x s

public export
lookup_function : Module nmfns -> Fin (S nmfns) -> FuncDef (S nmfns)
lookup_function (MkModule functions) fi = index fi functions

public export
assert_type : (t : Type') -> Value -> Result (idrisTypeOfType t)
assert_type TypeInt (ValueInt x) = Right x
assert_type TypeDouble (ValueFloat x) = Right x
assert_type t@TypeInt x@(ValueFloat x') = Left $ "Expected " ++ show t ++ " got " ++ show (typeOfValue x) ++ " (" ++ show x ++ ")"
assert_type t@TypeDouble x@(ValueInt x') = Left $ "Expected " ++ show t ++ " got " ++ show (typeOfValue x) ++ " (" ++ show x ++ ")"

public export
idris_val_to_value : idrisTypeOfType t -> Value
idris_val_to_value {t = TypeInt} x = ValueInt x
idris_val_to_value {t = TypeDouble} x = ValueFloat x

mutual
    public export
    interp_unop : (at : Type') -> (rt : Type') ->
                    ((idrisTypeOfType at) -> (idrisTypeOfType rt)) ->
                    Module nmfns -> State cd -> Expr cd (S nmfns) -> Result (Value, State cd)
    interp_unop at rt f mod state x = do
        (x_v, state') <- interp_expr mod state x
        x_n <- assert_type at x_v
        let r_n = f x_n
        pure (idris_val_to_value r_n, state')

    public export
    interp_binop : (at : Type') -> (rt : Type') ->
                    ((idrisTypeOfType at) -> (idrisTypeOfType at) -> (idrisTypeOfType rt)) ->
                    Module nmfns -> State cd -> Expr cd (S nmfns) -> Expr cd (S nmfns) -> Result (Value, State cd)
    interp_binop at rt f mod state x y = do
        (x_v, state') <- interp_expr mod state x
        x_n <- assert_type at x_v
        (y_v, state'') <- interp_expr mod state' y
        y_n <- assert_type at y_v
        let r_n = f x_n y_n
        pure (idris_val_to_value r_n, state'')

    public export
    interp_all_exprs : Module nmfns -> State cd -> List (Expr cd (S nmfns)) -> Result (List Value, State cd)
    interp_all_exprs mod state [] = Right ([], state)
    interp_all_exprs mod state (e :: es) = do
        (v, state') <- interp_expr mod state e
        (vs, state'') <- interp_all_exprs mod state' es
        pure (v :: vs, state'')

    public export
    interp_call : Module nmfns -> FuncDef (S nmfns) -> List Value -> Result Value
    interp_call mod (MkFuncDef returnType argumentTypes body) args =
        case decEq (length argumentTypes) (length args) of
            (No contra) => Left $ "Wrong number of arguments. Expected " ++ (show (length argumentTypes)) ++ " got " ++ (show (length args))
            (Yes prf) => case (map typeOfValue args) == argumentTypes of
                False => Left $ "Wrong argument type (somewhere)"
                True => do
                    let initialState = fromList args
                    (res, newState) <- interp_expr {cd=length argumentTypes} mod (rewrite prf in initialState) body
                    pure res

    public export
    interp_expr : Module nmfns -> State cd -> Expr cd (S nmfns) -> Result (Value, State cd)
    interp_expr mod state (ExprValue x) = Right (x, state)
    interp_expr mod state (ExprVar var) = Right (lookup_state state var, state)
    interp_expr mod state (ExprDeclareVar t initExpr after) = do
        (init_v, state') <- interp_expr mod state initExpr
        assert_type t init_v
        let state'' = init_v :: state'
        (v, state''') <- interp_expr mod state'' after
        pure (v, tail state''')
    interp_expr mod state (ExprUpdateVar var newExpr after) = do
        let old_t = typeOfValue (lookup_state state var)
        (new_v, state') <- interp_expr mod state newExpr
        assert_type old_t new_v

        let state'' = update_state state' var new_v
        interp_expr mod state'' after
    interp_expr mod state (ExprCall f args) = do
        (arg_vals, state') <- interp_all_exprs mod state args
        let f_def = lookup_function mod f
        v <- interp_call mod f_def arg_vals
        pure (v, state')
    interp_expr mod state (ExprIfNonZ cond true false) = do
        (cond_v, state') <- interp_expr mod state cond
        x <- assert_type TypeInt cond_v
        if x == 0
            then interp_expr mod state' false
            else interp_expr mod state' true
    interp_expr mod state (ExprWhileNonZ cond body after) = do
        (cond_v, state') <- interp_expr mod state cond
        x <- assert_type TypeInt cond_v
        if x == 0
            then interp_expr mod state' after
            else do
                (body_v, state'') <- interp_expr mod state' body
                interp_expr mod state'' (ExprWhileNonZ cond body after)
    interp_expr mod state (ExprIAdd x y) = interp_binop TypeInt TypeInt (+) mod state x y
    interp_expr mod state (ExprFAdd x y) = interp_binop TypeDouble TypeDouble (+) mod state x y
    interp_expr mod state (ExprISub x y) = interp_binop TypeInt TypeInt (-) mod state x y
    interp_expr mod state (ExprFSub x y) = interp_binop TypeDouble TypeDouble (-) mod state x y
    interp_expr mod state (ExprIMul x y) = interp_binop TypeInt TypeInt (*) mod state x y
    interp_expr mod state (ExprFMul x y) = interp_binop TypeDouble TypeDouble (*) mod state x y
    interp_expr mod state (ExprIDiv x y) = interp_binop TypeInt TypeInt div mod state x y
    interp_expr mod state (ExprFDiv x y) = interp_binop TypeDouble TypeDouble (/) mod state x y
    interp_expr modulue state (ExprIMod x y) = interp_binop TypeInt TypeInt mod modulue state x y
    interp_expr mod state (ExprIGT x y) = interp_binop TypeInt TypeInt (bool_to_int `comp2` (>)) mod state x y
    interp_expr mod state (ExprFGT x y) = interp_binop TypeDouble TypeInt (bool_to_int `comp2` (>)) mod state x y
    interp_expr mod state (ExprIGTE x y) = interp_binop TypeInt TypeInt (bool_to_int `comp2` (>=)) mod state x y
    interp_expr mod state (ExprFGTE x y) = interp_binop TypeDouble TypeInt (bool_to_int `comp2` (>=)) mod state x y
    interp_expr mod state (ExprIEQ x y) = interp_binop TypeInt TypeInt (bool_to_int `comp2` (==)) mod state x y
    interp_expr mod state (ExprFEQ x y) = interp_binop TypeDouble TypeInt (bool_to_int `comp2` (==)) mod state x y
    interp_expr mod state (ExprILTE x y) = interp_binop TypeInt TypeInt (bool_to_int `comp2` (<=)) mod state x y
    interp_expr mod state (ExprFLTE x y) = interp_binop TypeDouble TypeInt (bool_to_int `comp2` (<=)) mod state x y
    interp_expr mod state (ExprILT x y) = interp_binop TypeInt TypeInt (bool_to_int `comp2` (<)) mod state x y
    interp_expr mod state (ExprFLT x y) = interp_binop TypeDouble TypeInt (bool_to_int `comp2` (<)) mod state x y
    interp_expr mod state (ExprAnd x y) = interp_binop TypeInt TypeInt int_and mod state x y
    interp_expr mod state (ExprOr x y) = interp_binop TypeInt TypeInt int_or mod state x y
    interp_expr mod state (ExprNot x) = interp_unop TypeInt TypeInt int_not mod state x


public export
interp_module : Module nmfns -> Either String Value
interp_module (MkModule (main_f :: fs)) = interp_call (MkModule (main_f :: fs)) main_f []
