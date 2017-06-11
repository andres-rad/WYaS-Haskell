module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTripPF' :: (Show a, Read b) => a -> b
roundTripPF' = read . show

main = do
    print (roundTrip 4)
    print (roundTripPF 4)
    print (id 4)