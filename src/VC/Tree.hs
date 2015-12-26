{-# LANGUAGE OverloadedStrings #-}

module VC.Tree where

import           Filesystem.Path     (basename)
import           Shelly

import           Control.Monad.Extra

import           Prelude             hiding (FilePath)

check :: FilePath -> Sh [FilePath]
check dir = do
  names <- ls dir
  if checkVCDir names
    then return []
    else do
         (subdirs, files) <- partitionM test_d =<< ls dir
         unmanaged <- traverse check subdirs
         let  topFiles fps = case files of
                 [] -> fps
                 _ -> dir : fps
         return $ case summary unmanaged of
           AllManaged -> topFiles []
           NoneManaged -> [dir]
           SomeManaged -> topFiles $ concat unmanaged

checkVCDir :: [FilePath] -> Bool
checkVCDir = checkGitDir

checkGitDir :: [FilePath] -> Bool
checkGitDir = elem ".git" . map basename

data Managed = NoneManaged | AllManaged | SomeManaged

isManaged :: [filePath] -> Bool
isManaged [] = True
isManaged _ = False

isUnmanaged :: [filePath] -> Bool
isUnmanaged [_] = True
isUnmanaged _ = False

isHybrid :: [filePath] -> Bool
isHybrid [] = False
isHybrid [_] = False
isHybrid _ = True

summary :: [[filePath]] -> Managed
summary xs | all isManaged xs = AllManaged
summary xs | all isUnmanaged xs = NoneManaged
summary _ = SomeManaged
