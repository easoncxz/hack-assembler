
module Model where

import qualified Data.Text as T
import Data.Text (Text)

data HackRegister = RegD | RegM | RegA
  deriving (Eq, Show)

data HackAM = UseA | UseM
  deriving (Eq, Show)

data HackComputation
  = ComputeZero
  | ComputeOne
  | NegOne
  | JustD
  | JustR
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

data HackJump
  = NoJump | JGT | JEQ | JGE | JLT | JNE | JLE | JMP
  deriving (Eq, Show)

data HackInstruction
  = CompInstruction
    { instrDestination :: [HackRegister]
    , instrComputation :: HackComputation
    , instrAM          :: HackAM
    , instrJump        :: Maybe HackJump
    }
  | AddrInstruction AddrInstruction
  | LabelInstruction
    { instrLabel :: Text }
  deriving (Eq, Show)

data AddrInstruction
  = AddrLiteral
    { instrAddr :: Int }
  | AddrSymbol
    { instrAddrSymbol :: Text }
  deriving (Eq, Show)
