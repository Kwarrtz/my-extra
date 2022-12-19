{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Extra ( 
    (?), (??), (?.), (?<>),
    (***), (&&&),
    (&), on, (<&>), (<**>),
    (<.>),
    (.*), (.**), (.***), (.****), (.*****),
    (&.),( &.*), (&.**), (&.***), (&.****), (&.*****)
) where

import Data.Function ( (&), on )
import Data.Functor ( (<&>) )
import Control.Applicative ( (<**>) ) 

import Data.Maybe ( fromMaybe )

infixl 1 ?, ??
infixr 9 ?.
infixr 6 ?<>

infixr 3 ***, &&&

infixr 9 <.>
infixr 8 .*, .**, .***, .****, .*****
infixr 8 &., &.*, &.**, &.***, &.****, &.*****

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = do
    x' <- f x
    (x':) <$> iterateM f x'

(?) :: Bool -> a -> Maybe a
b ? x = if b then Just x else Nothing

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

(?.) :: Bool -> (a -> a) -> (a -> a)
b ?. f = if b then f else id

(?<>) :: Monoid a => Bool -> a -> a
b ?<> x = if b then x else mempty

(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f *** g) (x, y) = (f x, g y) 

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f &&& g) x = (f x, g x)

(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(f <.> g) a = f <$> g a

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) f g = \x y -> f (g x y)

(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) f g = \x y z -> f (g x y z)

(.***) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.***) f g = \w x y z -> f (g w x y z)

(.****) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.****) f g = \v w x y z -> f (g v w x y z)

(.*****) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.*****) f g = \u v w x y z -> f (g u v w x y z)

(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)

(&.*) :: (b -> c) -> (a -> c -> d) -> a -> b -> d
(&.*) f g = \x y -> g x (f y)

(&.**) :: (c -> d) -> (a -> b -> d -> e) -> a -> b -> c -> e
(&.**) f g = \x y z -> g x y (f z)

(&.***) :: (d -> e) -> (a -> b -> c -> e -> f) -> a -> b -> c -> d -> f
(&.***) f g = \w x y z -> g w x y (f z)

(&.****) :: (e -> f) -> (a -> b -> c -> d -> f -> g) -> a -> b -> c -> d -> e -> g
(&.****) f g = \v w x y z -> g v w x y (f z)

(&.*****) :: (f -> g) -> (a -> b -> c -> d -> e -> g -> h) -> a -> b -> c -> d -> e -> f -> h
(&.*****) f g = \u v w x y z -> g u v w x y (f z)