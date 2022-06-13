{-# LANGUAGE TupleSections #-}


module Main where


import           Control.Monad           (forM, foldM)
import           Data.String             (fromString)
import qualified Data.Text               as T
import           Data.Text               (Text)
import           PlutusTx                (ToData, Data)
import           PlutusBridge.SampleData
import           PlutusBridge.Utils
import           System.Environment      (getArgs)
import           System.FilePath.Posix   (takeBaseName)


main :: IO ()
main =
  let
    sampleCommand = 
      -- {{{
         "Here's and example set of arguments:\n"
      ++ "    cabal run plutus-bridge-app -- "
      ++       "path/to/output/file.js "
      ++       "path/to/makeDatum1.json "
      ++       "path/to/makeDatum2.json"
      -- }}}
    printHelp     =
      -- {{{
      putStrLn $
           "\n\n# PLUTUS BROWSER BRIDGE"
        ++ "\n\n"
        ++ "This is the executable that generates a Javascript module\n"
        ++ "containing functions that return `PlutusData` values (from\n"
        ++ "Emurgo's serialization library) from a list of JSON files.\n"
        ++ "The names of these files will be used as the resulting function\n"
        ++ "names."
        ++ "\n\n"
        ++ "Your sample values should be as inclusive as possible. For\n"
        ++ "example, if your custom datum has a list field, make sure that\n"
        ++ "it's not empty and contains at least one element."
        ++ "\n\n"
        ++ "These JSON files are presumed to have been generated using the\n"
        ++ "Cardano API library (JSON encoding of the `scriptDataToJSON`\n"
        ++ "function output)."
        ++ "\n\n"
        ++ sampleCommand
        ++ "\n\n"
        ++ "For a more extensive guide, refer to `README.md`."
        ++ "\n\n"
      -- }}}
  in do
  args <- getArgs
  case args of
    []                 ->
      -- {{{
      printHelp
      -- }}}
    ["h"]              ->
      -- {{{
      printHelp
      -- }}}
    ["-h"]             ->
      -- {{{
      printHelp
      -- }}}
    ["help"]           ->
      -- {{{
      printHelp
      -- }}}
    ["--help"]         ->
      -- {{{
      printHelp
      -- }}}
    ["man"]            ->
      -- {{{
      printHelp
      -- }}}
    [_]                ->
      -- {{{
      putStrLn $
        -- {{{
           "\n\nPlease provide a list of JSON files each representing a "
        ++ "custom datum/redeemer."
        ++ "\n\n"
        ++ sampleCommand
        ++ "\n\n"
        ++ "To view a brief guide, use any of the common manual/help "
        ++ "flags/arguments, or simply omit them altogether to view a brief "
        ++ "guide. For a more extensive one, refer to `README.md`."
        ++ "\n\n"
        -- }}}
      -- }}}
    fileName : jsFiles -> do
      -- {{{
      eitherSDs <- forM jsFiles (\jsFile -> (takeBaseName jsFile,) <$> parseJSON jsFile)
      let foldFn :: [SampleData]
                 -> (String, Either Text Data)
                 -> IO [SampleData]
          foldFn acc (fnName, eitherTextOrData) =
            -- {{{
            case eitherTextOrData of
              Right data' -> do
                -- {{{
                putStrLn $
                     "SUCCESSFULLY generated JS function for `"
                  ++ fnName
                  ++ "`."
                return $ makeSampleData' (fromString fnName) data' : acc
                -- }}}
              Left err    -> do
                -- {{{
                putStrLn $
                     "JS function generation FAILED for `"
                  ++ fnName
                  ++ "`: "
                  ++ (T.unpack err)
                return acc
                -- }}}
            -- }}}
      sampleDataList <- foldM foldFn [] eitherSDs
      generateModule fileName sampleDataList
      -- }}}
