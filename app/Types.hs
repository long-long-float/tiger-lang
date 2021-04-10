module Types where

data Type
  = Int
  | String
  -- | Record [(Symbol, Type)]
  | Array Type -- TODO: Add unique
  | Nil
  | Unit
  -- | Name
  deriving (Eq, Ord, Show)

