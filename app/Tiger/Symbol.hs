module Tiger.Symbol where

import Data.Text (Text)
import qualified Data.Map.Strict as M

data Symbol = Symbol { name :: Text, id :: Int }
  deriving (Eq, Ord, Show)
type SymbolTable = M.Map Text Int

