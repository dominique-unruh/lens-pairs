{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Use where

import Lib(pair, lensFst, lensSnd, Lens, chain, get, Compatible, PhFst, PhSnd)

l13 :: Lens a (a,(b,c)) _
l13 = lensFst

l23 :: Lens b (a,(b,c)) _
l23 = chain lensSnd lensFst

l33 :: Lens c (a,(b,c)) _
l33 = chain lensSnd lensSnd

test = pair (pair l13 l33) l23

instance {-# INCOHERENT #-} Compatible PhFst PhFst

bad = pair lensFst lensFst

main :: IO ()
main = print (get bad (1,2))
--main = print (get test (1,(2,3)))
