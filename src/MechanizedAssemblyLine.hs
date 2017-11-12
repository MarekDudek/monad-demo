module MechanizedAssemblyLine where

import Debug.Trace


type Wood = String
type Chopsticks = String
data Wrapped x = Wrapped x
    deriving (Eq, Show)


data Tray a = Empty | Contains a
    deriving (Eq, Show)

instance Functor Tray where
    fmap f (Contains x) = Contains (f x)

instance Applicative Tray where
    pure = Contains
    Contains f <*> Contains x = Contains (f x)
    _ <*> _ = Empty

instance Monad Tray where
    Empty        >>= _      = Empty
    (Contains x) >>= worker = worker x
    return = Contains
    fail _ = trace "! failed !" Empty


makeChopsticks :: Wood -> Tray Chopsticks
makeChopsticks w = 
    if null w 
        then trace "! got empty string !" Empty
        else Contains $ w ++ ", roughly chopped"

polishChopsticks :: Chopsticks -> Tray Chopsticks
polishChopsticks c = Contains $ c ++ ", polished"

wrapChopsticks :: Chopsticks -> Tray (Wrapped Chopsticks)
wrapChopsticks c = Contains (Wrapped (c ++ ", wrapped"))


assemblyLine :: Wood -> Tray (Wrapped Chopsticks)
assemblyLine w = 
    do 
        c <- makeChopsticks w
        p <- polishChopsticks c
        w <- wrapChopsticks p
        return w

assemblyLine' :: Wood -> Tray (Wrapped Chopsticks)
assemblyLine' w = (return w) >>= makeChopsticks >>= polishChopsticks >>= wrapChopsticks
