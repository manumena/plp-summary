module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

-- W(x)
infer' (VarExp x) n = OK (n+1, (
                                extendE emptyEnv x (TVar n),
                                (VarExp x),
                                (TVar n)
                                ))

-- W(0)
infer' (ZeroExp) n = OK (n, (
                                emptyEnv,
                                ZeroExp,
                                TNat
                                ))

-- W(true)
infer' (TrueExp) n = OK (n, (
                                emptyEnv,
                                TrueExp,
                                TBool
                                ))

-- W(false)
infer' (FalseExp) n = OK (n, (
                                emptyEnv,
                                FalseExp,
                                TBool
                                ))

-- W(succ(U))
infer' (SuccExp (e)) n = 
    case infer' e n of
        OK (n', (env', e', t')) ->
            case mgu [(t', TNat)] of
                UOK subst ->
                    OK (n', (
                            subst <.> env',
                            subst <.> SuccExp e',
                            TNat
                        ))
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

-- W(pred(U))
infer' (PredExp (e)) n = 
    case infer' e n of
        OK (n', (env', e', t')) ->
            case mgu [(t', TNat)] of
                UOK subst ->
                    OK (n', (
                            subst <.> env',
                            subst <.> PredExp e',
                            TNat
                        ))
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

-- W(iszero(U))
infer' (IsZeroExp (e)) n = 
    case infer' e n of
        OK (n', (env', e', t')) ->
            case mgu [(t', TNat)] of
                UOK subst ->
                    OK (n', (
                            subst <.> env',
                            subst <.> IsZeroExp e',
                            TBool
                        ))
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

-- W(if U then V else W)
infer' (IfExp u v w) n = 
    case infer' u n of
        OK (nu, (envu, u', tu)) ->
            case mgu [(tu, TBool)] of
                UOK subst ->
                    case infer' v nu of
                        OK (nv, (envv, v', tv)) ->
                            case infer' w nv of
                                OK (nw, (envw, w', tw)) ->
                                    case mgu [(tw, tv)] of
                                        UOK subst -> 
                                            OK (nw, (
                                                subst <.> joinE [envu, envv, envw], -- falta que no se contradicen
                                                subst <.> (IfExp u' v' w'),
                                                subst <.> tw
                                            ))
                                        UError u1 u2 ->
                                            uError u1 u2
                                res@(Error _) -> res
                        res@(Error _) -> res
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
