{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Grafana.Layout where

import Data.Bifunctor
import Data.Foldable

data Pos = Pos !Int !Int
  deriving (Eq, Show)

data Rect = Rect
  { left :: !Int
  , right :: !Int
  , top :: !Int
  , bottom :: !Int
  } deriving (Eq, Show)

instance Semigroup Rect where
  Rect l r t b <> Rect l' r' t' b' =
    Rect (min l l') (max r r') (min t t') (max b b')

data Layout a
  = Layout [(Rect, a)] Rect
  | Empty
  deriving (Eq, Show, Functor)

instance Semigroup (Layout a) where
  Layout ars ab <> Layout brs bb = Layout (ars <> brs) (ab <> bb)
  a <> Empty = a
  Empty <> a = a

instance Monoid (Layout a) where
  mempty = Empty

moveLayout :: Pos -> Layout a -> Layout a
moveLayout _ Empty = Empty
moveLayout p (Layout xs rect) =
  Layout
    (map (first (moveRect p)) xs)
    (moveRect p rect)

moveRect :: Pos -> Rect -> Rect
moveRect (Pos x y) (Rect l r t b) = Rect (l + x) (r + x) (t + y) (b + y)

atop :: Layout a -> Layout a -> Layout a
atop a b = a <> moveLayout (Pos 0 (bottoml a)) b

nextTo :: Layout a -> Layout a -> Layout a
nextTo a b = a <> moveLayout (Pos (rightl a) 0) b

rightl :: Layout a -> Int
rightl Empty = 0
rightl (Layout _ r) = right r

bottoml :: Layout a -> Int
bottoml Empty = 0
bottoml (Layout _ r) = bottom r

rowFits :: Int -> Int -> Int -> [Rect]
rowFits maxWidth itemCount rowHeight =
  fmap (\x -> Rect x (x+width) 0 rowHeight) xs
  where
    xs = [padding, width + padding..]
    width = maxWidth `div` itemCount
    padding = maxWidth `mod` itemCount `div` 2

fitToRow :: Int -> Int -> [Rect -> Layout a] -> Layout a
fitToRow maxWidth rowHeight items = fold $
  zipWith ($) items (rowFits maxWidth (length items) rowHeight)

fillRows :: Int -> Int -> Int -> [Rect -> Layout a] -> Layout a
fillRows maxWidth rowHeight cols items
  | null thisRow = Empty
  | otherwise =
      fold (zipWith ($) thisRow (rowFits maxWidth cols rowHeight))
        `atop` fillRows maxWidth rowHeight cols rest
  where
  (thisRow, rest) = splitAt cols items
