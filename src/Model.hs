
module Model where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

type SymbolTable = Map Text Int

data ParseState = ParseState
  { srcLineNo        :: Int
  , outLineNo        :: Int
  , symbolTable      :: SymbolTable
  , output           :: Seq Text
  , nextVariableAddr :: Int
  }
  deriving (Show)

data AsmError
  = SyntaxError Int String
  | ProgramError String
  deriving (Show)


data HackRegister = RegD | RegM | RegA
  deriving (Eq, Ord)

instance Show HackRegister where
  show RegD = "D"
  show RegM = "M"
  show RegA = "A"

data HackAM = UseA | UseM
  deriving (Eq, Show)

data HackComputation
  = ComputeZero
  | ComputeOne
  | ComputeNegOne
  | ComputeD
  | ComputeR
  | NotD
  | NotR
  | NegD
  | NegR
  | DPlusOne
  | RPlusOne
  | DSubOne
  | RSubOne
  | DPlusR
  | DSubR
  | RSubD
  | DAndR
  | DOrR
  deriving (Eq, Show)

data HackInstruction
  = CompInstruction
    { instrDestination :: Set HackRegister
    , instrComputation :: HackComputation
    , instrAM          :: HackAM
    , instrJump        :: Set Ordering
    }
  | AddrInstruction AddrInstruction
  | LabelInstruction
    { instrLabel :: Text }
  | EmptyInstruction
  deriving (Eq, Show)

data AddrInstruction
  = AddrLiteral
    { instrAddr :: Int }
  | AddrSymbol
    { instrAddrSymbol :: Text }
  deriving (Eq, Show)

