module Database where

data Root = Root Vet deriving Show

data Vet = Vet { name :: String, visits :: [Visit]} deriving Show

--data Visit = Visit { zipCode :: String, date :: String, sties :: [Sty] } deriving Show
data Visit = Visit { zipCode :: String, date :: String, sties :: [Pig] } deriving Show

data Sty = Sty { pigs :: [Pig] } deriving Show

data Pig = Pig { symptoms :: [Int], diagnose :: Either Int String } deriving Show

{-
root :: Root
root = Root $ Vet "Martijn"
                [ Visit "3581" "27-3-2009" $
                    [ Sty [ Pig [0,0,0] (Left 2) ]
                    , Sty [ Pig [0,1,1] (Right "Malaria")
                          , Pig [1,1,1] (Left 3) ]
                    ]
                , Visit "7612" "26-3-2009" $
                    [ Sty [ Pig [0,0,0] (Left 2) ]
                    ]
                ]
-}