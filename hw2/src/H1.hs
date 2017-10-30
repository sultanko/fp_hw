{-# LANGUAGE TypeOperators #-}

module H1 where

import           Control.Category (Category(..))
import           Control.Monad    (liftM2)
import           Data.Maybe       (fromJust, fromMaybe, isNothing)

data Expr
  = Const Int
  | Sum Expr
        Expr
  | Subtract Expr
             Expr
  | Multiply Expr
             Expr
  | Division Expr
             Expr
  | Power Expr
          Expr
  deriving (Show)

newtype ArithmeticError =
  ArithmeticError String
  deriving (Show, Eq)

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Sum l r) = liftM2 (+) (eval l) (eval r)
eval (Subtract l r) = liftM2 (-) (eval l) (eval r)
eval (Multiply l r) = liftM2 (*) (eval l) (eval r)
eval (Division l r) =
  if fromRight 1 rr == 0
    then Left (ArithmeticError "Division by zero")
    else liftM2 div (eval l) rr
  where
    rr = eval r
eval (Power l r) =
  if fromRight 1 rr < 0
    then Left (ArithmeticError "Negative power")
    else liftM2 (^) (eval l) rr
  where
    rr = eval r

data a ~> b
  = Partial (a -> Maybe b)
  | Defaulted (a ~> b)
              b

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial (\x -> Just (f x))

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) x = f x
apply (Defaulted f d) x = Just (fromMaybe d r)
  where
    r = apply f x

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f x d = fromMaybe d (apply f x)

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault (Defaulted f d) d2 = Defaulted f d2
withDefault f d = Defaulted f d

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f x = isNothing $ apply f x

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f1 f2 =
  Partial
    (\x ->
       if isDefinedAt f1 x
         then apply f1 x
         else apply f2 x)

instance Category (~>) where
  id = Partial Just
  f . g =
    partial
      (\x ->
         if isDefinedAt g x
           then Nothing
           else apply f (fromJust $ apply g x))
