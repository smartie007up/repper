{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: The Commandline interface for parsing Billboard data. See:
-- John Ashley Burgoyne, Jonathan Wild, Ichiro Fujinaga, 
-- /An Expert Ground-Truth Set for Audio Chord Recognition and Music Analysis/,
-- In: Proceedings of International Conference on Music Information Retrieval,
-- 2011. (<http://ismir2011.ismir.net/papers/OS8-1.pdf>) 
--------------------------------------------------------------------------------

module Main (main) where

import Billboard.BillboardData ( BBChord (..)
                               , reduceTimedBBChords, BillboardData(..)
                               , showFullChord, showInMIREXFormat, getTitle)
import Billboard.BillboardParser ( parseBillboard )
import Billboard.Tests ( mainTestFile, mainTestDir, rangeTest
                       , oddBeatLengthTest, getOffBeats) 
import Billboard.IOUtils 

-- harmtrace imports
import HarmTrace.Base.MusicTime (TimedData (..), dropTimed)

-- other libraries
import System.Console.ParseArgs
import System.FilePath
import Control.Monad (when, void)
import Text.Printf (printf)
import Data.List (genericLength, intercalate)

--------------------------------------------------------------------------------
-- Contants
--------------------------------------------------------------------------------

outputFileName :: FilePath
outputFileName = "mirex_chords" <.> "txt"

--------------------------------------------------------------------------------
-- Commandline argument parsing
--------------------------------------------------------------------------------
data ReppatArgs = InputFilepath | InputDirFilepath | InputID | ModeArg | OutDir
                | Compression deriving (Eq, Ord, Show)

myArgs :: [Arg ReppatArgs]
myArgs = [
           Arg { argIndex = ModeArg,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "mode" ArgtypeString,
                 argDesc  = "The operation mode (parse|mirex|test|full)"
               }
        ,  Arg { argIndex = OutDir,
                 argAbbr  = Just 'o',
                 argName  = Just "out",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Output directory for the mirex files"
               }               
         , Arg { argIndex = InputFilepath,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input file containing chord information"
               }
         , Arg { argIndex = InputDirFilepath,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Base directory path to the billboard dataset"
               }
         , Arg { argIndex = InputID,
                 argAbbr  = Just 'i',
                 argName  = Just "id",
                 argData  = argDataOptional "integer" ArgtypeInt,
                 argDesc  = (  "Input identifier (0-1000). Can only be used\n"
                            ++ "                          in combination with "
                            ++ "a base directory" )
               }
         , Arg { argIndex = Compression,
                 argAbbr  = Just 'c',
                 argName  = Just "comp",
                 argData  = argDataOptional "string" ArgtypeString,
                 argDesc  = (  "Use \"exp(and)\" to use eighth note grid output"
                            ++ "\n                          and \"red(uce)\" "
                            ++ "for compressed output" )
               }
         ]

-- representing the mode of operation
data Mode = Mirex | Parse | Test | Full | Result deriving (Eq)

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          -- check whether we have a usable mode
          let mode   = case (getRequiredArg arg ModeArg) of
                         "full"   -> Full
                         "mirex"  -> Mirex
                         "parse"  -> Parse
                         "test"   -> Test
                         "result" -> Result
                         m        -> usageError arg ("unrecognised mode: " ++ m)
              -- get filepaths           
              inFile = getArg arg InputFilepath
              inDir  = getArg arg InputDirFilepath
              bbid   = getArg arg InputID
              mout   = getArg arg OutDir
              compf  = case getArg arg Compression of
                        (Just "exp")    -> id
                        (Just "expand") -> id
                        (Just "red")    -> reduceTimedBBChords
                        (Just "reduce") -> reduceTimedBBChords
                        _               -> reduceTimedBBChords
              
          -- the input is either a file (Left) or a directory (Right)
          input  <-  case (inFile, inDir, bbid) of
                        -- we have two ways of identifying a file: by filename
                        (Just f , Nothing, Nothing) -> return (Left f)
                        -- or by basepath and id
                        (Nothing, Just d , Just i ) -> getBBFile d i 
                                                       >>= return . Left
                        (Nothing, Just d , Nothing) -> return (Right d)
                        _ -> usageError arg "Invalid filepaths" 
          
          -- do the parsing magic
          case (mode, input) of
            (Full, Left  f) -> showFile (showFullChord compf) f
            (Full, Right d) -> void $ writeDir (showFullChord compf) mout d
            (Mirex,  Left  f) -> showFile (showInMIREXFormat compf) f
            (Mirex,  Right d) -> void $ writeDir (showInMIREXFormat compf) mout d
            (Parse,  Left  f) -> parseFile compf f
            (Parse,  Right d) -> parseDir d
            (Test ,  Left  f) -> mainTestFile rangeTest f
            (Test ,  Right d) -> mainTestDir oddBeatLengthTest d
            (Result,  Left  _) -> error "can only run results on a full dataset"
            (Result,  Right d) -> runResults d
            

--------------------------------------------------------------------------------
-- Parsing Billboard data verbosely
-------------------------------------------------------------------------------- 

parseFile :: ([TimedData BBChord] -> [TimedData BBChord]) -> FilePath -> IO ()
parseFile cf fp = do inp <- readFile fp
                     let (bbd, err) = parseBillboard inp
                     printBillboard bbd
                     mapM_ print err where


  printBillboard :: BillboardData -> IO()
  printBillboard bd = 
    do putStrLn (getArtist bd ++ ": " ++ getTitle bd)
       putStr . concatMap (\x -> ' ' : show x) . dropTimed . cf . getSong $ bd
       putStr " |\n\n" 

-- parses a directory of Billboard songs
parseDir :: FilePath -> IO ()
parseDir d = void . bbdir oneSliceSalami $ d where
    -- parses a billboard file and presents the user with condenced output
    -- If parsing errors are encountered, they are printed
    oneSliceSalami :: FilePath -> IO ([TimedData BBChord])
    oneSliceSalami f = 
      do inp <- readFile f
         let (bbd, err) = parseBillboard inp
             s          = getSong bbd
         putStrLn (getArtist bbd ++ ": " ++ getTitle bbd)
         putStrLn (d </> "salami_chords.txt, beats: " ++ show (length s) 
                  ++ ", errors: "                     ++ show (length err))
         when (not $ null err) (mapM_ print err)
         return s

--------------------------------------------------------------------------------
-- Converting the Billboard format to MIREX format
--------------------------------------------------------------------------------

-- Reads a file and prints the chords in mirex format
showFile :: (BillboardData -> String) -> FilePath -> IO ()
showFile shwf f = readFile f >>= putStrLn . shwf . fst . parseBillboard

-- Reads a directory an writes a file with the chords, in a format defined
-- by a specific show function, in the folder containing also the original file
writeDir :: (BillboardData -> String) -> Maybe FilePath -> FilePath 
         -> IO [String]
writeDir shwf mfp d = getBBFiles d >>= mapM toMirex where

  -- read, parses, and writes one mirex chord file
  toMirex :: (FilePath, Int) -> IO (String)
  toMirex (f,i) = -- (filename, id)
    do inp <- readFile f
       let (bbd, err) = parseBillboard inp
           s          = shwf bbd
           out        = case mfp of 
                          -- place the file next to the input file (but rename)
                          Nothing -> dropFileName f </> outputFileName
                          -- place the output file in a specific folder (if set)
                          Just fp -> fp </>  printf "%.4d" i ++ "_audio.lab"
       when (not $ null err) (error ("there were errors in file: " ++ f))
       writeFile out s
       putStrLn ("written file: " ++ out)
       return s

runResults :: FilePath -> IO ()
runResults d = do let ths = [0.05, 0.075, 0.1, 0.15, 0.25]
                  putStrLn $ intercalate "\t" ("File" : map show ths)
                  getBBFiles d >>= mapM_ (getResult ths)
  where 
    getResult :: [Double] -> (FilePath, Int) -> IO [Double]
    getResult ths (fp, _) = do inp <- readFile fp 
                                   >>= return . getSong . fst . parseBillboard
                               let r = map (offRatio inp) ths
                               putStrLn $ intercalate "\t" (fp : map show r)
                               return r
                               
    offRatio :: [TimedData BBChord] -> Double -> Double
    offRatio td th = genericLength (getOffBeats th td) / genericLength td 
                           
