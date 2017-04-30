
module Model where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

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
