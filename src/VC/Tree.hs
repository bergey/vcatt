{-# LANGUAGE OverloadedStrings #-}

module VC.Tree where

import           Filesystem.Path     (basename)
import           Shelly

import           Control.Monad.Extra

import           Prelude             hiding (FilePath)

check :: FilePath -> Sh Managed
check dir = do
  names <- ls dir
  if checkVCDir names
    then return Managed
    else do
    -- TODO exclude symlinks from files
         (subdirs, files) <- partitionM test_d =<< ls dir
         managed <- traverse check subdirs
         return $ summary dir (files == []) managed

checkVCDir :: [FilePath] -> Bool
checkVCDir xs = let names = map basename xs in
  any (`elem` names)
  [ ".git", "_darcs", ".hg", ".svn", "CVS", ".fslckout"]

data Managed =
  UnManaged FilePath
  | Managed
  | Empty
  | SomeManaged [FilePath]

summary :: FilePath -> Bool -> [Managed] -> Managed
summary dir nofiles xs
  | all isEmpty xs = if nofiles
                     then Empty
                     else UnManaged dir
  | all isManaged xs = if nofiles then Managed
                       else UnManaged dir
  | all isUnmanaged xs = UnManaged dir
  | otherwise = SomeManaged $ if nofiles
                then concatMap directories xs
                else dir : concatMap directories xs

isEmpty :: Managed -> Bool
isEmpty Empty = True
isEmpty _ = False

isManaged :: Managed-> Bool
isManaged Managed = True
isManaged Empty = True
isManaged _ = False

isUnmanaged :: Managed -> Bool
isUnmanaged (UnManaged _) = True
isUnmanaged Empty = True
isUnmanaged _ = False

directories :: Managed -> [FilePath]
directories (SomeManaged ds) = ds
directories (UnManaged d) = [d]
directories _ = []
