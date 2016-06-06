{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.IOUtils
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set Billboard specific file and directory utilities
--------------------------------------------------------------------------------

module Billboard.IOUtils (bbdir, getBBFiles, getBBFile) where

import System.Directory
import System.FilePath
import Text.Printf (printf)
import Control.Monad (filterM)

-- | Applies a function to all files in a directory
bbdir :: (FilePath -> IO a) ->  FilePath -> IO [a]
bbdir f fp =   getDirectoryContents fp >>= filterM isBillboardDir 
           >>= mapM (\d -> f (fp </> d </> "salami_chords.txt")) 

-- | Given the path to the Billboard collection, returns a list with the
-- filepaths and id's of the salami_chords.txt files. (The id is the parent
-- folder name.)
getBBFiles :: FilePath -> IO [(FilePath, Int)]
getBBFiles p =   getDirectoryContents p >>= filterM isBillboardDir  
             >>= mapM (\d -> return (p </> d </> "salami_chords.txt", read d)) 

-- Returns true if the folder name is 4 characters long and a number between 
-- 0 and 1000
isBillboardDir :: String -> IO Bool
isBillboardDir x = case length x of
  4 -> do let n = read x :: Int -- x should be a folder 0 - 1000
          return (n >= 0 && n <= 1000)
  _ -> return False

-- | Given a base directory pointing to the billboard location and a billboard
-- id, this function returns the path to that particular billboard file. If
-- the file does not exist, an error is thrown.
getBBFile :: FilePath -> Int -> IO (FilePath)
getBBFile billboardLoc nr = 
  do let  fp = billboardLoc </> printf "%04d" nr </> "salami_chords.txt"
     fpExist <- doesFileExist fp
     case fpExist of
       True  -> return fp 
       False -> error ("Error: " ++ printf "%04d" nr 
                  ++ " is not a valid billboard id, or the directory " 
                  ++ billboardLoc ++"does not point to the billboard collection"
                  ++ " Regardless, the file " ++ fp ++ " does not exist" )
