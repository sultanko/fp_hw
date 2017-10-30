{-# LANGUAGE NoImplicitPrelude #-}

module H3Classes(Const(..), Pair(..), Tree(..), Identity(..), Either(..), Maybe(..)) where

import           Prelude (Show (..))

newtype Const a b = Const { getConst :: a } deriving (Show)

newtype Pair a b = Pair (a, b) deriving (Show)

newtype Identity a = Identity { runIdentity :: a } deriving (Show)

data Either a b = Left a | Right b deriving (Show)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

data Maybe a = Nothing | Just a deriving Show
