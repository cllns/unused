module Unused.LikelihoodCalculatorSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.Types
import Unused.LikelihoodCalculator
import Unused.ResultsClassifier

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "calculateLikelihood" $ do
        it "prefers language-specific checks first" $ do
            let railsMatches = [ TermMatch "ApplicationController" "app/controllers/application_controller.rb" 1 ]
            removalLikelihood' railsMatches `shouldReturn` Low

            let elixirMatches = [ TermMatch "AwesomeView" "web/views/awesome_view.ex" 1 ]
            removalLikelihood' elixirMatches `shouldReturn` Low

        it "weighs widely-used methods as low likelihood" $ do
            let matches = [ TermMatch "full_name" "app/models/user.rb" 4
                          , TermMatch "full_name" "app/views/application/_auth_header.rb" 1
                          , TermMatch "full_name" "app/mailers/user_mailer.rb" 1
                          , TermMatch "full_name" "spec/models/user_spec.rb" 10
                          ]

            removalLikelihood' matches `shouldReturn` Low

        it "weighs only-used-once methods as high likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" 1 ]

            removalLikelihood' matches `shouldReturn` High

        it "weighs methods that seem to only be tested and never used as high likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" 1
                          , TermMatch "obscure_method" "spec/models/user_spec.rb" 5
                          ]

            removalLikelihood' matches `shouldReturn` High

        it "weighs methods that seem to only be tested and used in one other area as medium likelihood" $ do
            let matches = [ TermMatch "obscure_method" "app/models/user.rb" 1
                          , TermMatch "obscure_method" "app/controllers/user_controller.rb" 1
                          , TermMatch "obscure_method" "spec/models/user_spec.rb" 5
                          , TermMatch "obscure_method" "spec/controllers/user_controller_spec.rb" 5
                          ]

            removalLikelihood' matches `shouldReturn` Medium

        it "doesn't mis-categorize allowed terms from different languages" $ do
            let matches = [ TermMatch "t" "web/models/foo.ex" 1 ]

            removalLikelihood' matches `shouldReturn` High

removalLikelihood' :: [TermMatch] -> IO RemovalLikelihood
removalLikelihood' ms = do
    (Right config) <- loadConfig
    return $ rLikelihood $ trRemoval $ calculateLikelihood config $ resultsFromMatches ms
