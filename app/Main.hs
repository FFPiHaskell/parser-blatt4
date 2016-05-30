module Main where

import Lib
import Data.Text as T

main :: IO ()
main = do
        f <- readFile "data/CSV_Dummy_simple.csv"
        case parseCSV f of
          Left err  -> putStrLn err
          Right csv -> putStrLn $ unpack $ T.unlines $ T.intercalate (pack "\t") <$> csv
