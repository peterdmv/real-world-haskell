module Main where

import Prettify (pretty)
import PrettyJSON
import SimpleJSON

main :: IO ()
main = putStrLn $ pretty 20 value
  where value = renderJValue (JObject [("f", JNumber 1), ("q", JBool True)])

