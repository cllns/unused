{-# LANGUAGE OverloadedStrings #-}

module Unused.GitContext
    ( withGitHistory
    ) where

import qualified Data.Text as T
import Data.Map.Strict as Map (toList, fromList)
import Control.Arrow ((&&&))
import Control.Concurrent.ParallelIO
import System.Process
import Unused.Types (TermMatchSet, TermResults(trGitContext), GitContext(..), GitCommit(..), RemovalLikelihood(High), totalOccurrenceCount, removalLikelihood)

withGitHistory :: Int -> TermMatchSet -> IO TermMatchSet
withGitHistory _ tms =
    Map.fromList <$> (parallel . map retrieve) listTerms <* stopGlobalPool
  where
    listTerms = Map.toList tms

retrieve :: (String, TermResults) -> IO (String, TermResults)
retrieve a@(token, results) =
    case (totalOccurrenceCount &&& removalLikelihood) results of
        (1, High) -> do
            gitContext <- logToGitContext <$> gitLogSearchFor token
            return (token, results { trGitContext = Just gitContext })
        (_, _) -> return a

-- 58e219e Allow developer-authored configurations
-- 307dd20 Introduce internal yaml configuration of auto low likelihood match handling
-- 3b627ee Allow multiple matches with single-occurring appropriate tokens
-- f7a2e1a Add Hspec and tests around parsing
logToGitContext :: String -> GitContext
logToGitContext =
    GitContext . map GitCommit . shaList
  where
    shaList = map (T.unpack . head . T.splitOn " " . T.pack) . lines

gitLogSearchFor :: String -> IO String
gitLogSearchFor t = do
  (_, results, _) <- readProcessWithExitCode "git" ["log", "-G", t, "--oneline", "-n", "2"] ""
  return results
