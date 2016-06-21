module Unused.CLI.Views.SearchResult
    ( ResultsFormat(..)
    , searchResults
    ) where

import Control.Monad (forM_, void)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.List (intercalate, (\\))
import Unused.Types
import Unused.Grouping (Grouping(..), GroupedTerms)
import Unused.CLI.Views.SearchResult.ColumnFormatter
import Unused.CLI.Util
import qualified Unused.CLI.Views.NoResultsFound as V

data ResultsOptions = ResultsOptions
    { roColumnFormat :: ColumnFormat
    , roOutputFormat :: ResultsFormat
    }

data ResultsFormat = Column | List
type ResultsPrinter = ReaderT ResultsOptions IO

searchResults :: ResultsFormat -> [GroupedTerms] -> IO ()
searchResults format terms = do
    resetScreen
    runReaderT (printFormattedTerms terms) resultsOptions
  where
    columnFormat = buildColumnFormatter $ termsToResults terms
    resultsOptions = ResultsOptions columnFormat format
    termsToResults = concatMap (Map.elems . snd)

printFormattedTerms :: [GroupedTerms] -> ResultsPrinter ()
printFormattedTerms [] = liftIO V.noResultsFound
printFormattedTerms ts = mapM_ printGroupingSection ts

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

printGroupingSection :: GroupedTerms -> ResultsPrinter ()
printGroupingSection (g, tms) = do
    liftIO $ printGrouping g
    mapM_ printTermResults $ listFromMatchSet tms

printGrouping :: Grouping -> IO ()
printGrouping NoGrouping = return ()
printGrouping g = do
    putStr "\n"
    setSGR [SetColor Foreground Vivid Black]
    setSGR [SetConsoleIntensity BoldIntensity]
    print g
    setSGR [Reset]

printTermResults :: (String, TermResults) -> ResultsPrinter ()
printTermResults =
    uncurry printMatches . (id &&& trMatches) . snd

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green
likelihoodColor Unknown = Black
likelihoodColor NotCalculated = Magenta

printMatches :: TermResults -> [TermMatch] -> ResultsPrinter ()
printMatches r ms = do
    outputFormat <- roOutputFormat <$> ask
    case outputFormat of
        Column -> printTable r ms
        List -> printList r ms

printList :: TermResults -> [TermMatch] -> ResultsPrinter ()
printList r ms = do
    liftIO $ forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (termColor r)]
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr "  "
        setSGR [SetUnderlining SingleUnderline]
        putStr $ tmTerm m
        setSGR [Reset]

        setSGR [SetColor Foreground Vivid Cyan]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr " ("
        putStr $ pluralize (totalFileCount r) "file" "files"
        putStr ", "
        putStr $ pluralize (totalOccurrenceCount r) "occurrence" "occurrences"
        putStr ")"
        setSGR [Reset]
        putStr "\n"

        if null remainingAliases
            then void $ putStr ""
            else do
                printHeader "    Aliases: "
                putStrLn $ intercalate ", " $ remainingAliases

        printHeader "    File Path: "
        setSGR [SetColor Foreground Dull Cyan]
        putStrLn $ tmPath m
        setSGR [Reset]

        case shas of
            Nothing -> void $ putStr ""
            Just shas' -> do
                printHeader "    Recent SHAs: "
                putStrLn $ intercalate ", " shas'

        printHeader "    Reason: "
        putStrLn $ removalReason r
        putStr "\n"
  where
    termColor = likelihoodColor . rLikelihood . trRemoval
    removalReason = rReason . trRemoval
    remainingAliases = trTerms r \\ [trTerm r]
    printHeader v = do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStr v
        setSGR [SetConsoleIntensity NormalIntensity]
    shas = (map gcSha . gcCommits) <$> trGitContext r

pluralize :: Int -> String -> String -> String
pluralize i@1 singular _ = show i ++ " " ++ singular
pluralize i _ plural = show i ++ " " ++ plural

printTable :: TermResults -> [TermMatch] -> ResultsPrinter ()
printTable r ms = do
    cf <- roColumnFormat <$> ask
    let printTerm = cfPrintTerm cf
    let printPath = cfPrintPath cf
    let printNumber = cfPrintNumber cf

    liftIO $ forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (termColor r)]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "     " ++ printTerm (tmTerm m)
        setSGR [Reset]

        setSGR [SetColor Foreground Vivid Cyan]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "  " ++ printNumber (totalFileCount r) ++ ", " ++ printNumber (totalOccurrenceCount r)
        setSGR [Reset]

        setSGR [SetColor Foreground Dull Cyan]
        setSGR [SetConsoleIntensity FaintIntensity]
        putStr $ "  " ++ printPath (tmPath m)
        setSGR [Reset]

        putStr $ "  " ++ removalReason r
        putStr "\n"
  where
    termColor = likelihoodColor . rLikelihood . trRemoval
    removalReason = rReason . trRemoval
