{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Conversion where

import Data.DList ( empty, snoc, toList )
import Core ( normalRedex, reduce, Resolution(Result) )
import qualified Prims as P
import Language

type Fuel = Int

-- | A thing is convertible when we can turn it into a lambda expression and can
-- turn well-formed expressions back into a pure type.
class Convertible a where
    encode :: a -> UntypedExpr
    decode :: UntypedExpr -> Fuel -> Maybe a


-- | Untyped expressions are trivially convertible.
instance Convertible UntypedExpr where
    encode e = e
    decode e _ = Just e


-- | A nat n is encoded as a function taking two arguments f and x, and computes
-- n applications of f on x. In order to decode, we let f be a typed successor
-- function, and x is typed constant zero.
instance Convertible Nat where
    encode (Nat n) = ULam "f" (ULam "x" (foldr (\_ e -> UApp "f" e) (UVar "x") [1..n]))
    decode n fuel =
        let incrementNatExpr :: TypedExpr Int
            incrementNatExpr = TypedFunc (\n -> Just (Typed (n + 1)))

            zeroNatExpr :: TypedExpr Int
            zeroNatExpr = Typed 0

            e = toTyped n # incrementNatExpr # zeroNatExpr in
                case reduce normalRedex fuel e of
                    Result (Typed x) -> Just (Nat x)
                    _ -> Nothing


-- | A boolean is encoded as a function taking two arguments, and returning
-- the first when true, and the second when false. In order to decode, we
-- just apply the encoded boolean to the typed constants true and false.
instance Convertible Bool where
    encode True = ULam "t" (ULam "f" "t")
    encode False = ULam "t" (ULam "f" "f")

    decode b fuel =
        let e = toTyped b # Typed True # Typed False in
            case reduce normalRedex fuel e of
                Result (Typed x) -> Just x
                _ -> Nothing


-- | A pair is encoded as a function that takes a selector, which returns
-- either the first or second component. In order to decode, we apply the pair
-- to a function which takes it as an argument - inside, we apply the head and
-- tail of the pair to a curried untyped function (which takes the head and tail)
-- and tries to decode the head and tail, returning the two as a pair upon
-- success.
instance (Convertible a, Convertible b) => Convertible (Pair a b) where
    encode (Pair (a, b)) = ULam "s" ("s" ## encode a ## encode b)

    decode p fuel =
        let pairExpr = Lam "p" ((UntypedFunc $ \a ->
                Just . UntypedFunc $ \b ->
                    case (decode a fuel, decode b fuel) of
                        (Just a', Just b') -> Just (Typed (a', b'))
                        _ -> Nothing) 
                # (toTyped P.head # "p")
                # (toTyped P.tail # "p"))

            e = pairExpr # toTyped p in 
                case reduce normalRedex fuel e of
                    Result (Typed x) -> Just $ Pair x
                    _ -> Nothing


-- | A list is encoded as a pair, where the first element is the head of the list,
-- and the second is the rest of the list - an empty list is represented as a 
-- function that always returns true (this is for determining if a list is empty).
-- In order to decode, it might occur to you that a right fold would be more
-- natural; given an encoded list [x1,...,xn] we could decode it as 
-- foldr cons [x1,..,xn] [] = cons x1 (cons x2 ... (cons xn [])...). Unfortunately,
-- because we are using the normal evaluation order, this applies the
-- outermost cons first, which doesn't end up evaluating the rest of the list.
-- Thus, we do foldl snoc [x1,...,xn] [] (using difference lists for efficiency).
instance Convertible a => Convertible [a] where
    encode xs = foldr (\x acc -> ULam "s" (encode x ## acc)) P.nil xs

    decode xs fuel =
        let emptyDListExpr = Typed empty
            snocDListExpr = TypedFunc (\xs ->
                Just (UntypedFunc (\e ->
                    Typed . snoc xs <$> decode e fuel)))

            e = toTyped P.foldl # snocDListExpr # emptyDListExpr # toTyped xs in
                case reduce normalRedex fuel e of
                    Result (App (Typed l2) _) -> Just $ toList l2
                    _ -> Nothing

-- For the evaluation function

instance Convertible (Annotated Nat) where
    encode (ANatResult x) = encode x

    decode n fuel = ANatResult <$> decode n fuel

instance Convertible (Annotated Bool) where
    encode (ABoolResult x) = encode x

    decode n fuel = ABoolResult <$> decode n fuel

instance Convertible (Annotated a) => Convertible (Annotated [Annotated a]) where
    encode (AListResult x) = encode x

    decode n fuel = AListResult <$> decode n fuel

instance (Convertible (Annotated a), Convertible (Annotated b)) => 
    Convertible (Annotated (Pair (Annotated a) (Annotated b))) where
    encode (APairResult x) = encode x

    decode n fuel = APairResult <$> decode n fuel


instance Convertible (Annotated UntypedExpr) where
    encode (AUntypedResult e) = e

    decode e fuel = AUntypedResult <$> (decode e fuel :: Maybe UntypedExpr)

-- Reduce an evaluation. This is possible because we have the annotated datatype,
-- so we can specify the type we would like the expression to be decoded to,
-- selected by the type in the TypedEvaluation. There may be a better way to
-- accomplish this with DataKinds or something.
evaluate :: Fuel -> Evaluation -> Maybe TypedResult
evaluate fuel ev = case ev of
    UntypedEvaluation e -> removeAnnotation <$> (decode e fuel :: Maybe (Annotated UntypedExpr))
    TypedEvaluation e ty -> case ty of
        NatType -> removeAnnotation <$> (decode e fuel :: Maybe (Annotated Nat))
        BoolType -> removeAnnotation <$> (decode e fuel :: Maybe (Annotated Bool))
        ListType ty1 ->
            case decode e fuel :: Maybe [UntypedExpr] of
                Just xs -> 
                    ListResult <$> traverse (\e1 -> evaluate fuel (TypedEvaluation e1 ty1)) xs
                _ -> Nothing
        PairType ty1 ty2 ->
            case decode e fuel :: Maybe (Pair UntypedExpr UntypedExpr) of
                Just (Pair (x, y)) ->
                    case (evaluate fuel (TypedEvaluation x ty1), evaluate fuel (TypedEvaluation y ty2)) of
                        (Just x', Just y') -> Just (PairResult (Pair (x', y')))
                        _ -> Nothing
                _ -> Nothing