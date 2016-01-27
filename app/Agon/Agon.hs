
module Agon.Agon where

import Agon.Types

import Control.Lens
import Control.Monad.Reader

type AgonM = ReaderT Agon

viewA lens = ask >>= return . view lens

runAgonM = runReaderT
