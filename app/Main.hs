{-# LANGUAGE OverloadedStrings #-}

module Main where

import Models                 (Person(..), DigitalTest(..), Course(..))
import Data.Aeson.Text        (encodeToLazyText)
import Data.List as L         (filter, zip, concatMap, head, tail)
import Data.Text.Lazy as T    (Text, intercalate, lines, null, splitOn)
import Data.Text.Lazy.IO as I (readFile, writeFile, putStrLn)


main :: IO ()
main = do
  csv <- I.readFile "persons.csv"

  let json = createPersonDataFromFile csv

  I.writeFile "json.txt" $ "[" <> json <> "]"

createPersonDataFromFile :: Text -> Text
createPersonDataFromFile csv = do
  let file     = L.concatMap parseRow $ parseCsv csv
  let topRow   = getRow file
  let nextRows = getNextRows file
  let json     = T.intercalate "," $ encodeToLazyText . tupleToPerson <$> zipAllRowsRecursively topRow nextRows

  json

tupleToPerson :: [(Text, Text)] -> Person
tupleToPerson xs = Person name email number role org group image link desc
  where name                = getValueFromKey "Name"
        email               = getValueFromKey "Email"
        number              = getValueFromKey "Number"
        role                = getValueFromKey "Role"
        org                 = getValueFromKey "Organisation"
        group               = getValueFromKey "Group"
        image               = getValueFromKey "Image URL"
        link                = getValueFromKey "Link"
        desc                = getValueFromKey "Description"
        getValueFromKey key = lookupKey key xs

lookupKey:: Text -> [(Text, Text)] -> Text
lookupKey key [] =  ""
lookupKey key (x: xs)
  | key == fst x = snd x
  | otherwise    = lookupKey key xs

parseCsv :: Text -> [[Text]]
parseCsv file = T.splitOn "\r" <$> T.lines file

parseRow :: [Text] -> [[Text]]
parseRow []     = []
parseRow [x]    = T.splitOn "," <$> [x]
parseRow (x: _) = T.splitOn "," <$> [x]

getRow :: [[Text]] -> [Text]
getRow = L.head

filterRow :: [Text] -> [Text]
filterRow = L.filter (not . T.null)

getNextRows :: [[Text]] -> [[Text]]
getNextRows = L.tail

zipRowsAsTuple :: [Text] -> [Text] -> [(Text, Text)]
zipRowsAsTuple = L.zip

zipAllRowsRecursively :: [Text] -> [[Text]] -> [[(Text, Text)]]
zipAllRowsRecursively topRow []      = []
zipAllRowsRecursively topRow [x]     = [zipRowsAsTuple topRow x]
zipAllRowsRecursively topRow (x: xs) = zipRowsAsTuple topRow x : zipAllRowsRecursively topRow xs
