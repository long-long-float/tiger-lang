module Tiger.Symbol where

import Data.Text (Text, pack, unpack)
import qualified Data.Map.Strict as M

data Symbol = Symbol { name :: Text, id :: Int }
  deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol name id) =
    if name == (pack "") then "(empty)"
    else (unpack name) ++ "@" ++ (show id)

type SymbolTable = M.Map Text Int

emptySymbol = Symbol (pack "") (-1)

