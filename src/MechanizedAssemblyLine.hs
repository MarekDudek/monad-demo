module MechanizedAssemblyLine where

import Debug.Trace


type Wood       = String
type Chopsticks = String

data Wrapped x = Wrapped x
    deriving (Eq, Show)

-- Tray 1

data Tray a = Contains a | Empty 
    deriving (Eq, Show)

instance Functor Tray where
    f `fmap `(Contains x) = Contains (f x)

instance Applicative Tray where
    pure = Contains
    Contains f <*> Contains x = Contains (f x)
    _ <*> _ = Empty

instance Monad Tray where
    Empty        >>= _      = Empty
    (Contains x) >>= worker = worker x
    return = Contains
    fail _ = trace "! failed !" Empty -- TODO: when is it used?


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
        c <- makeChopsticks   w
        p <- polishChopsticks c
        w <- wrapChopsticks   p
        return w

assemblyLine' :: Wood -> Tray (Wrapped Chopsticks)
assemblyLine' w = (return w) >>= makeChopsticks >>= polishChopsticks >>= wrapChopsticks

-- Tray 2

data Tray2 x = Contains2 x | Failed2 String
    deriving (Eq, Show)

instance Functor Tray2 where
    f `fmap` (Contains2 x) = Contains2 (f x)

instance Applicative Tray2 where
    pure = Contains2
    Contains2 f <*> Contains2 x = Contains2 (f x)
    Contains2 f <*> Failed2 x   = Failed2 x
    Failed2 f   <*> Failed2 x   = Failed2 (f ++ " " ++ x)
    Failed2 f   <*> Contains2 x = Failed2 f

instance Monad Tray2 where
    (Failed2 reason) >>= _      = Failed2 reason
    (Contains2 x)    >>= worker = worker x
    return = Contains2
    fail reason = trace "! failed !" (Failed2 reason) -- TODO: when is it used?

makeChopsticks2 :: Wood -> Tray2 Chopsticks
makeChopsticks2 w = 
    if null w 
        then Failed2 "wood was empty"
        else Contains2 $ w ++ ", roughly chopped" -- adds 17 characters

polishChopsticks2 :: Chopsticks -> Tray2 Chopsticks
polishChopsticks2 c = 
    if length c `mod` 3 /= 0
        then Contains2 $ c ++ ", polished" -- adds 10 characters
        else Failed2 "number of chopsticks was divisible by three"

wrapChopsticks2 :: Chopsticks -> Tray2 (Wrapped Chopsticks)
wrapChopsticks2 c = 
    if length c `mod` 4 /= 0
        then Contains2 (Wrapped (c ++ ", wrapped"))
        else Failed2 "number of chopsticks was divisible by four"


assemblyLine2 :: Wood -> Tray2 (Wrapped Chopsticks)
assemblyLine2 w = 
    do 
        c <- makeChopsticks2   w
        p <- polishChopsticks2 c
        w <- wrapChopsticks2   p
        return w

assemblyLine2' :: Wood -> Tray2 (Wrapped Chopsticks)
assemblyLine2' w = (return w) >>= makeChopsticks2 >>= polishChopsticks2 >>= wrapChopsticks2

-- independent from concrete monad

makeChopsticks3 :: (Monad m) => Wood -> m Chopsticks
makeChopsticks3 w = return $ w ++ ", roughly chopped"

polishChopsticks3 :: (Monad m) => Chopsticks -> m Chopsticks
polishChopsticks3 c = return $ c ++ ", polished"

wrapChopsticks3 :: (Monad m) => Chopsticks -> m (Wrapped Chopsticks)
wrapChopsticks3 c = return (Wrapped (c ++ ", wrapped"))

assemblyLine3 :: (Monad m) => Wood -> m (Wrapped Chopsticks)
assemblyLine3 w = 
    do 
        c <- makeChopsticks3   w
        p <- polishChopsticks3 c
        w <- wrapChopsticks3   p
        return w
