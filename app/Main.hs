{-# LANGUAGE OverloadedStrings #-}

module Main where

import           VC.Tree

import           Shelly

import           Control.Monad
import           Data.Foldable
import qualified Data.Text          as T
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let startingPath = case args of
        [] -> "."
        (d:_) -> fromText $ T.pack d
  shelly $ do
    fps <- directories <$> check startingPath
    traverse_ (echo <=< toTextWarn) fps
