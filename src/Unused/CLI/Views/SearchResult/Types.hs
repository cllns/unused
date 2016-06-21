module Unused.CLI.Views.SearchResult.Types
    ( ResultsOptions(..)
    , ResultsFormat(..)
    , ResultsPrinter
    , ColumnFormat(..)
    , R.ask
    , R.runReaderT
    , M.liftIO
    ) where

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.IO.Class as M
import Unused.CLI.Views.SearchResult.ColumnFormatter

data ResultsOptions = ResultsOptions
    { roColumnFormat :: ColumnFormat
    , roOutputFormat :: ResultsFormat
    }

data ResultsFormat = Column | List
type ResultsPrinter = R.ReaderT ResultsOptions IO
