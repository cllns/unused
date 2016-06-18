{-# LANGUAGE OverloadedStrings #-}

module Unused.GitContext
    ( gitContextForResults
    ) where

import qualified Data.Text as T
import qualified Data.List as L
import System.Process
import Unused.Types (TermResults(trGitContext), GitContext(..), GitCommit(..), RemovalLikelihood(High), removalLikelihood, resultAliases)

gitContextForResults :: Int -> (String, TermResults) -> IO [(String, TermResults)]
gitContextForResults commitCount a@(token, results) =
    case removalLikelihood results of
        High -> do
            gitContext <- logToGitContext <$> gitLogSearchFor commitCount (resultAliases results)
            return [(token, results { trGitContext = Just gitContext })]
        _ -> return [a]

-- 58e219e Allow developer-authored configurations
-- 307dd20 Introduce internal yaml configuration of auto low likelihood match handling
-- 3b627ee Allow multiple matches with single-occurring appropriate tokens
-- f7a2e1a Add Hspec and tests around parsing
logToGitContext :: String -> GitContext
logToGitContext =
    GitContext . map GitCommit . shaList
  where
    shaList = map (T.unpack . head . T.splitOn " " . T.pack) . lines

gitLogSearchFor :: Int -> [String] -> IO String
gitLogSearchFor commitCount ts = do
  (_, results, _) <- readProcessWithExitCode "git" ["log", "-G", L.intercalate "|" ts, "--oneline", "-n", show commitCount] ""
  return results
