{-# LANGUAGE RecordWildCards #-}

module Nameable where

import Client

class Nameable n where
  name :: n -> String

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
         = f ++ " " ++ n
  name c = clientName c

instance Eq i => Ord (Client i) where
  compare c1 c2 = case compare (name c1) (name c2) of
                    LT -> LT
                    GT -> GT
                    EQ -> case (c1, c2) of
                            (Individual { .. }, _) -> GT
                            (_, Individual { .. }) -> LT
                            _                      -> compare (duty c1)
                                                              (duty c2)
