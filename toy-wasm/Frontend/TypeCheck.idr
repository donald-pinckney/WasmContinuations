module TypeCheck

import Frontend.Parser

import LangDefs.ToyAST

import Utils

import Data.Vect
import Data.Fin

%default covering

lookup_context : Vect cd (String, Type') -> String -> Either String (Fin cd, Type')
lookup_context [] y = Left $ "use of undeclared variable: " ++ y
lookup_context ((x, t) :: xs) y =
    if x == y
        then Right (FZ, t)
        else do
            (idx, t') <- lookup_context xs y
            pure (FS idx, t')

lookup_fn : Vect fns (String, Type', List Type') -> String -> Either String (Fin fns, Type', List Type')
lookup_fn [] y = Left $ "use of undeclared function: " ++ y
lookup_fn ((x, rt, pts) :: xs) y =
    if x == y
        then Right (FZ, rt, pts)
        else do
            (idx, rt', pts') <- lookup_fn xs y
            pure (FS idx, rt', pts')

assert_eq_types : Type' -> Type' -> Either String ()
assert_eq_types x y = if x == y then pure () else Left $ "Expected type: " ++ show x ++ ", received: " ++ show y

mutual
    find_type : (funcTypes : Vect fns (String, Type', List Type')) -> (context : Vect cd (String, Type')) -> PExpr -> Either String (Type', Expr cd fns)
    find_type fns context x = case check_type fns context x TypeInt of
        (Left err1) => case check_type fns context x TypeDouble of
            (Left err2) => case check_type fns context x TypeBool of
                (Left err3) => case check_type fns context x TypeUnit of
                    (Left err4) => Left "no valid type found"
                    (Right r) => Right (TypeUnit, r)
                (Right r) => Right (TypeBool, r)
            (Right r) => Right (TypeDouble, r)
        (Right r) => Right (TypeInt, r)

    check_binary : (op : Expr cd fns -> Expr cd fns -> Expr cd fns) ->
                    (funcTypes : Vect fns (String, Type', List Type')) ->
                    (context : Vect cd (String, Type')) ->
                    Type' -> PExpr -> PExpr -> Either String (Expr cd fns)
    check_binary op fns context t x y = [op x' y' | x' <- check_type fns context x t, y' <- check_type fns context y t]

    check_type : (funcTypes : Vect fns (String, Type', List Type')) -> (context : Vect cd (String, Type')) -> PExpr -> Type' -> Either String (Expr cd fns)
    check_type fns context (PExprVar x) t = do
        (debruijn, vt) <- lookup_context context x
        assert_eq_types t vt
        pure $ ExprVar debruijn
    check_type fns context (PExprValue x) t = do
        assert_eq_types t (typeOfValue x)
        pure $ ExprValue x
    check_type fns context (PExprDeclareVar vt name initExpr after) t = do
        initExpr' <- check_type fns context initExpr vt
        afterExpr' <- check_type fns ((name, vt) :: context) after t
        case eq_unit vt of
            (Yes prf) => Left "Can not declare variable of unit type."
            (No contra) => pure $ ExprDeclareVar vt contra initExpr' afterExpr'
    check_type fns context (PExprUpdateVar name updateExpr after) t = do
        (debruijn, vt) <- lookup_context context name
        updateExpr' <- check_type fns context updateExpr vt
        afterExpr' <- check_type fns context after t
        pure $ ExprUpdateVar debruijn updateExpr' afterExpr'
    check_type fns context (PExprCall name args) t = do
        (f_idx, rt, pts) <- lookup_fn fns name
        assert_eq_types t rt
        if length pts /= length args
            then Left ("Expected " ++ show (length pts) ++ " arguments, received " ++ show (length args))
            else do
                let pts_args = zip pts args
                args' <- (mapExcept (\pt_arg => check_type fns context (snd pt_arg) (fst pt_arg)) pts_args)
                pure $ ExprCall f_idx (fst args')
    check_type fns context (PExprIf cond tr fa) t =
        [ExprIf cond' t tr' fa' |
            cond' <- check_type fns context cond TypeBool,
            tr' <- check_type fns context tr t,
            fa' <- check_type fns context fa t
        ]
    check_type fns context (PExprWhile cond body after) t =
        [ExprWhile cond' body' after' |
            cond' <- check_type fns context cond TypeBool,
            body' <- check_type fns context body TypeUnit,
            after' <- check_type fns context after t
        ]
    check_type fns context (PExprAdd x y) TypeInt = check_binary ExprIAdd fns context TypeInt x y
    check_type fns context (PExprAdd x y) TypeDouble = check_binary ExprFAdd fns context TypeDouble x y
    check_type fns context (PExprAdd x y) TypeBool = Left "+ does not return bool"
    check_type fns context (PExprAdd x y) TypeUnit = Left "+ does not return unit"
    check_type fns context (PExprSub x y) TypeInt = check_binary ExprISub fns context TypeInt x y
    check_type fns context (PExprSub x y) TypeDouble = check_binary ExprFSub fns context TypeDouble x y
    check_type fns context (PExprSub x y) TypeBool = Left "- does not return bool"
    check_type fns context (PExprSub x y) TypeUnit = Left "- does not return unit"
    check_type fns context (PExprNeg x) TypeInt = [ExprINeg x' | x' <- check_type fns context x TypeInt]
    check_type fns context (PExprNeg x) TypeDouble = [ExprFNeg x' | x' <- check_type fns context x TypeDouble]
    check_type fns context (PExprNeg x) TypeBool = Left "- does not return bool"
    check_type fns context (PExprNeg x) TypeUnit = Left "- does not return unit"
    check_type fns context (PExprMul x y) TypeInt = check_binary ExprIMul fns context TypeInt x y
    check_type fns context (PExprMul x y) TypeDouble = check_binary ExprFMul fns context TypeDouble x y
    check_type fns context (PExprMul x y) TypeBool = Left "* does not return bool"
    check_type fns context (PExprMul x y) TypeUnit = Left "* does not return unit"
    check_type fns context (PExprDiv x y) TypeInt = check_binary ExprIDiv fns context TypeInt x y
    check_type fns context (PExprDiv x y) TypeDouble = check_binary ExprFDiv fns context TypeDouble x y
    check_type fns context (PExprDiv x y) TypeBool = Left "/ does not return bool"
    check_type fns context (PExprDiv x y) TypeUnit = Left "/ does not return unit"
    check_type fns context (PExprMod x y) TypeInt = check_binary ExprIMod fns context TypeInt x y
    check_type fns context (PExprMod x y) TypeDouble = Left "% does not return float"
    check_type fns context (PExprMod x y) TypeBool = Left "% does not return bool"
    check_type fns context (PExprMod x y) TypeUnit = Left "- does not return unit"
    check_type fns context (PExprGT x y) TypeBool = do
        (x_t, x') <- find_type fns context x
        if x_t == TypeBool || x_t == TypeUnit then Left "can not compare bools or units" else do
            y' <- check_type fns context y x_t
            pure $ (if x_t == TypeInt then ExprIGT else ExprFGT) x' y'
    check_type fns context (PExprGT x y) t = Left $ "> does not return " ++ show t
    check_type fns context (PExprGTE x y) TypeBool = do
        (x_t, x') <- find_type fns context x
        if x_t == TypeBool || x_t == TypeUnit then Left "can not compare bools or units" else do
            y' <- check_type fns context y x_t
            pure $ (if x_t == TypeInt then ExprIGTE else ExprFGTE) x' y'
    check_type fns context (PExprGTE x y) t = Left $ ">= does not return " ++ show t
    check_type fns context (PExprEQ x y) TypeBool = do
        (x_t, x') <- find_type fns context x
        if x_t == TypeBool || x_t == TypeUnit then Left "can not compare bools or units" else do
            y' <- check_type fns context y x_t
            pure $ (if x_t == TypeInt then ExprIEQ else ExprFEQ) x' y'
    check_type fns context (PExprEQ x y) t = Left $ "== does not return " ++ show t
    check_type fns context (PExprLTE x y) TypeBool = do
        (x_t, x') <- find_type fns context x
        if x_t == TypeBool || x_t == TypeUnit then Left "can not compare bools or units" else do
            y' <- check_type fns context y x_t
            pure $ (if x_t == TypeInt then ExprILTE else ExprFLTE) x' y'
    check_type fns context (PExprLTE x y) t = Left $ "<= does not return " ++ show t
    check_type fns context (PExprLT x y) TypeBool = do
        (x_t, x') <- find_type fns context x
        if x_t == TypeBool || x_t == TypeUnit then Left "can not compare bools or units" else do
            y' <- check_type fns context y x_t
            pure $ (if x_t == TypeInt then ExprILT else ExprFLT) x' y'
    check_type fns context (PExprLT x y) t = Left $ "< does not return " ++ show t
    check_type fns context (PExprAnd x y) TypeBool = check_binary ExprAnd fns context TypeBool x y
    check_type fns context (PExprAnd x y) t = Left $ "&& does not return " ++ show t
    check_type fns context (PExprOr x y) TypeBool = check_binary ExprOr fns context TypeBool x y
    check_type fns context (PExprOr x y) t = Left $ "|| does not return " ++ show t
    check_type fns context (PExprNot x) TypeBool = [ExprNot x' | x' <- check_type fns context x TypeBool]
    check_type fns context (PExprNot x) t = Left $ "! does not return " ++ show t
    check_type fns context (PExprCast x t') t = do
        assert_eq_types t t'
        (x_t, x') <- find_type fns context x
        pure (ExprCast x' x_t t)

check_all_not_unit : (ts : List Type') -> Either String (out_ts : List (t : Type' ** Not (t = TypeUnit)) ** length out_ts = length ts)
check_all_not_unit xs = mapExcept (\t => case eq_unit t of
    (Yes prf) => Left "Can not have a unit as a function parameter"
    (No contra) => Right (t ** contra)
) xs

typeCheckFunc : (funcTypes : Vect fns (String, Type', List Type')) -> PFunc -> Either String (FuncDef fns)
typeCheckFunc funcTypes (MkPFunc name returnType params body) =
    let context = reverse (fromList params) in
    if not (allUnique (toList (map fst context)))
        then Left "Error: duplicate parameter names"
        else do
            body' <- check_type funcTypes context body returnType
            (param_types ** same_len) <- check_all_not_unit (map snd params)
            pure $ MkFuncDef returnType param_types (rewrite same_len in rewrite mapPreservesLength Prelude.Basics.snd params in body')

getFuncType : PFunc -> (String, Type', List Type')
getFuncType (MkPFunc name returnType params body) = (name, returnType, map snd params)

export
typeCheckModule : (fns : (PFunc, List PFunc)) -> Either String (Module (length (snd fns)))
typeCheckModule (f, fs) =
    let funcs = f :: fromList fs in
    let types = map getFuncType funcs in
    [MkModule checked_fns | checked_fns <- mapExcept (typeCheckFunc types) funcs]
