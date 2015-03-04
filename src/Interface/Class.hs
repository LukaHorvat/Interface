{-# LANGUAGE MultiParamTypeClasses #-}
module Interface.Class where

class Interface c i where
    i :: c -> i

infixl 1 -->

(-->) :: a -> (a -> b) -> b
(-->) a b = b a
