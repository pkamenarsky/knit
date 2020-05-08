{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module DatatypeSpec where

import           Data.Char
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Development.Shake
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Silently
import           Test.Hspec hiding (Selector)
import           Test.Hspec.QuickCheck
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Generics.Eot

spec :: Spec
spec = modifyMaxSize (const 20) $ modifyMaxSuccess (const 20) $ do
  describe "datatype" $ do
    around_ silence $ it "works for every ADT" $ do
      property $ \ dt -> test dt [i|
        {-# LANGUAGE DeriveGeneric #-}

        import Generics.Eot

        main = print $ datatype (Proxy :: Proxy #{datatypeName dt})

      |]

  describe "toString" $ do
    it "works" $ do
      property $ \ dt ->
        (not $ null $ constructors dt) ==>
        test dt [i|
          {-# LANGUAGE DeriveAnyClass #-}
          {-# LANGUAGE DeriveGeneric #-}
          {-# LANGUAGE StandaloneDeriving #-}

          import Prelude (Int, Show(..), (/=), ($), error, (++), unlines)
          import Generics.Eot
          import Control.Monad

          import ToString

          main = do
            when (toString testValue /= show testValue) $
              error $ unlines $
                "" :
                ("expected: " ++ show (show testValue)) :
                ("got:      " ++ show (toString testValue)) :
                []

          deriving instance ToString #{datatypeName dt}
          deriving instance Show #{datatypeName dt}

        |]

test :: Datatype -> String -> Property
test dt testCode = do
  property $ \ n -> hasUniqueNames dt ==> ioProperty $ do
    current <- getCurrentDirectory
    inTempDirectory $ do
      let code = unindent testCode ++ "\n" ++
            toDatatypeDecl dt n
      writeFile "Main.hs" code
      (Stderr output, exitCode) <-
        cmd "runhaskell"
          ("-i" ++ current </> "src")
          ("-i" ++ current </> "examples")
          "Main.hs"
      return $
        counterexample code $
        counterexample output $
        (exitCode == ExitSuccess)

toDatatypeDecl :: Datatype -> Int -> String
toDatatypeDecl (Datatype n []) _ = unindent [i|
  data #{n}
    deriving (Generic)
  |]
toDatatypeDecl dt n = unindent [i|
  data #{datatypeName dt}
    = #{intercalate " | " (map renderCons (constructors dt))}
    deriving (Generic)

  testValue :: #{datatypeName dt}
  testValue = #{testValue}
  |]
  where
    renderCons :: Constructor -> String
    renderCons = \ case
      Constructor name (Selectors fields) ->
        let render n = [i|#{n} :: Int|]
        in [i|#{name} { #{intercalate ", " (map render fields)} }|]
      Constructor name (NoSelectors n) ->
        [i|#{name} #{intercalate " " (replicate n "Int")}|]
      Constructor name NoFields -> name

    conss = constructors dt

    testValue :: String
    testValue = case conss !! (abs n `mod` length conss) of
      Constructor name fields ->
        unwords (name : replicate (numberOfFields fields) "42")

    numberOfFields :: Fields -> Int
    numberOfFields = \ case
      Selectors sels -> length sels
      NoSelectors n -> n
      NoFields -> 0

hasUniqueNames :: Datatype -> Bool
hasUniqueNames (Datatype _ conss) =
  unique (map constructorName conss) &&
  all uniqueCons (map fields conss)
  where
    unique :: Eq a => [a] -> Bool
    unique l = nub l == l

    uniqueCons = \ case
      Selectors fields -> unique fields
      _ -> True

upperCase :: [Char]
upperCase = ['A' .. 'Z']

lowerCase :: [Char]
lowerCase = ['a' .. 'z']

allLetters :: [Char]
allLetters = lowerCase ++ upperCase

arbitraryLowerName :: Gen String
arbitraryLowerName = flip suchThat (not . isKeyword) $ do
  first <- elements lowerCase
  (first :) <$> listOf (elements allLetters)

shrinkLowerName :: String -> [String]
shrinkLowerName s =
  filter isValid (shrink s)
  where
    isValid w@(s : _) = isLower s && not (isKeyword w)
    isValid [] = False

isKeyword :: String -> Bool
isKeyword s = s `elem` keywords
  where
    keywords =
      words "case class data default deriving do else foreign if import in infix infixl infixr instance let module newtype of then type where"

arbitraryUpperName :: Gen String
arbitraryUpperName = do
  first <- elements upperCase
  (first :) <$> listOf (elements allLetters)

shrinkUpperName :: String -> [String]
shrinkUpperName s =
  filter isValid (shrink s)
  where
    isValid (s : _) = isUpper s
    isValid [] = False

instance Arbitrary Datatype where
  arbitrary = Datatype <$> arbitraryUpperName <*> arbitrary
  shrink (Datatype n cs) =
    [Datatype n' cs | n' <- shrinkUpperName n] ++
    [Datatype n cs' | cs' <- shrink cs]

instance Arbitrary Constructor where
  arbitrary = Constructor <$> arbitraryUpperName <*> arbitrary
  shrink (Constructor n fs) =
    [Constructor n' fs | n' <- shrinkUpperName n] ++
    [Constructor n fs' | fs' <- shrink fs]

instance Arbitrary Fields where
  arbitrary = oneof $
    return NoFields :
    (Selectors <$> listOf arbitraryLowerName) :
    (NoSelectors <$> arbitrary) :
    []
  shrink = \ case
    NoFields -> []
    NoSelectors i ->
      NoFields :
      map NoSelectors (shrink i)
    Selectors fs ->
      NoSelectors (length fs) :
      nub (map Selectors (shrinkList shrinkLowerName fs))
