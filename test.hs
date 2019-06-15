

------------------------------------
-- ZuriHac 2019
-- Beginner Track Approximate Notes
-- Taught by Ryan Moore, Thank You!
-- ~ Simon
------------------------------------


test x = x+2

f x =
    let
        y = 5
    in
        x+y


data ZS = ZuriSingle

data ZuriContainer a =
        ZuriContainer a a deriving Show

data ZuriSum =
    ZuriOne | ZuriTwo deriving Show -- unit types / sum types

data ZuriSum1 a = ZuriA | ZuriB a deriving Show


ff value count = ff (value+value) (count-1)

-- *Main> show . Cell 4 . Cell 3 $ Cell 2 Empty



data ZuriList a = 
    Empty | Cell a (ZuriList a) -- deriving Show

instance (Show a) => Show (ZuriList a) where
    show Empty = "Empty"
    show (Cell x xs) = "Cell "++ show x ++" : "++show xs

-- :i show ?

myShow ZuriA = "ZuriA"
myShow (ZuriB a) = "ZuriB"

-- *Main> myShow $ ZuriB 4

zuriHead :: ZuriList a -> Maybe a
zuriHead Empty = Nothing
zuriHead (Cell x xs) = Just x

zuriTail :: ZuriList a -> Maybe (ZuriList a)
zuriTail Empty = Nothing
zuriTail (Cell _ xs) = Just xs

zuriLast :: ZuriList a -> Maybe a
zuriLast Empty = Nothing
zuriLast (Cell x Empty) = Just x 
zuriLast (Cell x xs) = zuriLast xs -- _

zuriNextToLast :: ZuriList a -> Maybe a
zuriNextToLast Empty = Nothing
-- zuriNextToLast (Cell x Empty) = Nothing
zuriNextToLast (Cell x (Cell y Empty)) = Just x
zuriNextToLast (Cell x xs) = zuriNextToLast xs -- _

-- *Main> let a = Cell 4 . Cell 3 $ Cell 2 Empty
zuriLength :: ZuriList a -> Integer
zuriLength Empty = 0
zuriLength (Cell _ xs) = 1+ zuriLength xs


zuriLength' :: ZuriList a -> Integer
zuriLength' xs = go 0 xs
    where
        go c Empty = c
        go c (Cell x xs) = go (c+1) xs

-- monoid
    -- empty value and assoc. bin. operator

-- import Data.Semigroup as S

instance Semigroup (ZuriList a) where
    (<>) xs Empty = xs
    (<>) Empty ys = ys
    (<>) (Cell y (Empty)) xs = Cell y xs
    (<>) (Cell z bs) xs = Cell z (bs <> xs) 

-- (Cell 1 Empty) <> Cell 2 (Cell 3 Empty)

-- import qualified Data.Monoid as M

instance Monoid (ZuriList a) where
    mappend = (<>)
    mempty = Empty

-- mconcat [a, b]

instance Functor ZuriList where
    fmap f Empty = Empty
    fmap f (Cell x xs) =
        Cell (f x) (fmap f xs)

-- fmap (\x->x+1) a

-- <*>

-- (Cell +3 (Cell +4 Empty)) <*> (Cell 2 (Cell 7 Empty))
instance Applicative ZuriList where
    pure x = Cell x Empty
    (<*>) Empty xs = Empty
    (<*>) xs Empty = Empty
    (<*>) (Cell f fs) xs = mappend ((<*>) fs xs) (fmap f xs)

-- note: mappend

-- "flatmap"
join :: ZuriList (ZuriList a) -> ZuriList a
join Empty = Empty
join (Cell xs xss) = xs <> (join xss)


instance Monad ZuriList where
    xs >>= f = join (fmap f xs)


-- "do syntax is just syntactic sugar"


-- Note: readfile imported automatically by prelude

-- doSomeIO = do
--     fileContents <- readfile "bla.txt"
--     putStrLn fileContents
--     let firstCharacter = head fileContents
--     return firstCharacter
