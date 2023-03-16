{-# LANGUAGE DeriveGeneric #-}

module Models (Person(..),
               Course(..),
               DigitalTest(..)
              ) where

import GHC.Generics   (Generic)
import Data.Aeson     (ToJSON)
import Data.Text.Lazy (Text)

data Person = Person
  { name :: Text
  , email :: Text
  , number :: Text
  , role :: Text
  , organisation :: Text
  , group :: Text
  , imageUrl :: Text
  , link :: Text
  , description :: Text
  } deriving (Generic, Show)
instance ToJSON Person

data Course = Course
  { role' :: Text
  , courseGroup :: Text
  , courseGroupIcon :: Text
  , courseName :: Text
  , courseCode :: Text
  , moduleCode :: Text
  , moduleName :: Text
  , pageTitle :: Text
  , pageIcon :: Text
  , htmlUrl :: Text
  , headerImageUrl :: Text
  , videoId :: Text
  , colour :: Text
  } deriving (Generic, Show)
instance ToJSON Course

data DigitalTest = DigitalTest
  { courseGroup' :: Text
  , courseCode' :: Text
  , digitalTestTitle :: Text
  , digitalTestIcon :: Text
  , confidenceTestTitle :: Text
  , confidenceTestIcon :: Text
  , moduleCode' :: Text
  , role'' :: Text
  , questionsFile :: Text
  } deriving (Generic, Show)
instance ToJSON DigitalTest
