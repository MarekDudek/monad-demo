module MechanizedAssemblyLine where


type Wood = String
type Chopsticks = String
data Wrapped x = Wrapped x
    deriving Show


data Tray a = Empty | Contains a
    deriving Show

instance Functor Tray where
    fmap f (Contains x) = Contains (f x)

instance Applicative Tray where
    pure = Contains
    Contains f <*> Contains x = Contains (f x)

instance Monad Tray where
    Empty >>= _ = Empty
    (Contains x) >>= worker = worker x
    return = Contains
    fail _ = Empty


makeChopsticks :: Wood -> Tray Chopsticks
makeChopsticks w = Contains $ w ++ ", roughly chopped"

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

