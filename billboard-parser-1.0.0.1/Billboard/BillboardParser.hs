{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -Wall           #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.BillboardParser
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of combinator parsers that parse Billboard data. See:
-- John Ashley Burgoyne, Jonathan Wild, Ichiro Fujinaga, 
-- /An Expert Ground-Truth Set for Audio Chord Recognition and Music Analysis/,
-- In: Proceedings of International Conference on Music Information Retrieval,
-- 2011. (<http://ismir2011.ismir.net/papers/OS8-1.pdf>) 
--------------------------------------------------------------------------------

module Billboard.BillboardParser ( pBillboard
                                 , parseBillboard
                                 , acceptableBeatDeviationMultiplier ) where

import Data.List (genericLength, partition)
import Control.Arrow (first)
-- import Control.Monad.State
import Text.ParserCombinators.UU

import HarmTrace.Base.Parsing hiding (pLineEnd)
import HarmTrace.Base.MusicRep hiding (isNone)
import HarmTrace.Base.MusicTime( TimedData (..), timedData
                               , BarTime (..), onset, offset)
import HarmTrace.Base.ChordTokenizer (pRoot, pChord)

import Billboard.BeatBar  ( TimeSig  (..), BeatWeight (..), tatumsPerBar
                          , chordsPerDot )
import Billboard.BillboardData 
import Billboard.Annotation (  Annotation (..), Label (..)
                            , Instrument (..), Description (..), isStart
                            , isRepeat, getRepeats)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | A parameter that sets the acceptable beat deviation multiplier, which
-- controls when exceptionally long beat lengths will be interpolated.
acceptableBeatDeviationMultiplier :: Double
acceptableBeatDeviationMultiplier = 0.075

--------------------------------------------------------------------------------
-- Top level Billboard parsers
--------------------------------------------------------------------------------

-- | Toplevel function for parsing Billboard data files. The function returns
-- a tuple containing the result of the parsing in a 'BillboardData' type and
-- a (possibly empty) list of parsing errors.
parseBillboard :: String ->  (BillboardData, [Error LineColPos])
parseBillboard = parseDataWithErrors pBillboard

-- | The top-level parser for parsing the billboard data (see 'parseBillboard').
pBillboard :: Parser BillboardData
pBillboard = do (a, t, ts, r) <- pHeader
                c             <- pChordLinesPost ts
                -- before we return the chords we store the index of the 
                -- chord in the 'BBChord' type (setChordIxsT)
                return (BillboardData a t ts r (setChordIxsT c)) where

             
--------------------------------------------------------------------------------
-- parsing meta data in the headers
--------------------------------------------------------------------------------

-- parsing meta data on the title, artist, metre and the tonic of the piece
pHeader :: Parser (Title, Artist, TimeSig, Root)
pHeader = sortMetas <$> (pMetaPrefix *> pTitle  ) <* pLineEnd <*>
                        (pMetaPrefix *> pArtist ) <* pLineEnd <*>
                        (pMetaPrefix *> pMeta   ) <* pLineEnd <*>
                        (pMetaPrefix *> pMeta   ) <* pLineEnd <*  pLineEnd where
                        
  sortMetas :: Title -> Artist -> Meta -> Meta -> (Title, Artist, TimeSig, Root)
  sortMetas t a (Metre  ts) (KeyRoot r) = (t, a, ts, r)
  sortMetas t a (KeyRoot r) (Metre  ts) = (t, a, ts, r)
  sortMetas _ _ _ _ = 
    error "pBillboard (sortMetas): no valid metre and tonic found"

pMetaPrefix :: Parser Char
pMetaPrefix = pSym '#' <* pSym ' '  
                    
pTitle   :: Parser Title
pTitle   = id <$> (pString "title: "  *> pReadableStr)

pArtist  :: Parser Artist
pArtist  = id <$> (pString "artist: " *> pReadableStr)

pMeta    :: Parser Meta
pMeta    = pMetre <|> pKeyRoot

pMetre   :: Parser Meta
pMetre   = Metre   <$> (pString "metre: "  *> pTimeSig    )

pKeyRoot :: Parser Meta
pKeyRoot = KeyRoot <$> (pString "tonic: "  *> pRoot       )

-- parses a time signature                    
pTimeSig :: Parser TimeSig
pTimeSig = TimeSig <$> (tuple <$> pIntegerRaw <*> (pSym '/' *> pIntegerRaw))

--------------------------------------------------------------------------------
-- parsing the additional annotations
--------------------------------------------------------------------------------

pStructStart :: Parser [Annotation]
pStructStart = pList (pStrucLab <|> pStrucDescStart)
               
-- structural annotations, e.g. A, B, C, etc..
pStrucLab :: Parser Annotation
pStrucLab = Start <$> (Struct <$> pUpper <*> pPrimes) <* pString ", "

-- recognises an arbitrary number of primes
pPrimes :: Parser Int
pPrimes = length <$> pMany (pSym '\'')

-- parses structural descriptions (strings before a ',' ) before the 
-- chord sequences
pStrucDescStart :: Parser Annotation
pStrucDescStart =  (Start . Anno) <$> pAnno <* pString ", "

-- parses a known or an unknown annotation
pAnno :: Parser Description
pAnno = pAnnotation <<|> pUnknownAnno 

-- the annotations known to this parser
pAnnotation :: Parser Description
pAnnotation =   Chorus         <$ pString "chorus"<* pMabSpcDsh <* pMaybe pLower
            <|> Verse          <$> (pString "verse" *> pMabSpc *> pMaybe pTextNr)
            <|> PreVerse       <$ pString "pre" <* pMabSpcDsh <* pString "verse"
            <|> Vocal          <$ pString "vocal"
            <|> Intro          <$ pMaybe (pString "pre") <* pMabSpcDsh 
                               <* pString "intro" <* pMabSpcDsh <* pMaybe pLower
            <|> Outro          <$ pString "outro"
            <|> Bridge         <$ pString "bridge"
            <|> Interlude      <$ pString "interlude" 
            <|> Transition     <$ pString "trans"<* pMaybe (pString "ition" )
            <|> Fadeout        <$ pString "fade" <* pMabSpc <* pString "out" 
            <|> Fadein         <$ pString "fade" <* pMabSpc <* pString "in" 
            <|> Solo           <$ pString "solo" 
            <|> Prechorus      <$ pString "pre"<* pMabSpcDsh <* pString "chorus"
            <|> Maintheme      <$ pString "main"<* pMabSpcDsh <* pString "theme"
            <|> Keychange      <$ pString "key"<* pMabSpcDsh <* pString "change"
            <|> Secondarytheme <$ pOptWrapPar "secondary" <* pMabSpcDsh 
                                                      <* pString "theme"
            <|> Instrumental   <$ pString "instrumental" 
                               <* pMaybe (pString " break")
            <|> Coda           <$ pString "coda"
            <|> Ending         <$ pString "ending"
            <|> Talking        <$ pString "spoken" <* pMaybe (pString " verse")
            <|> ModulationSeg  <$ pString "modulation"

pTextNr :: Parser Int
pTextNr =   1 <$ pString "one"
        <|> 2 <$ pString "two"
        <|> 3 <$ pString "three"
        <|> 4 <$ pString "four"
        <|> 5 <$ pString "five"
        <|> 6 <$ pString "six"
        <|> 7 <$ pString "seven"
        <|> 8 <$ pString "eight"
        <|> 9 <$ pString "nine"

pUnknownAnno :: Parser Description
pUnknownAnno = UnknownAnno <$> pList1 pLower

-- parses after the chords sequence description
pEndAnnotations :: Parser [Annotation]
pEndAnnotations = (++) <$> pRepeat <*> ((++) <$> pPhrase <*> pEndAnno) 

pEndAnno :: Parser [Annotation]
pEndAnno = concat <$> pList (pString ", " *>  
                            (pPhrase <|> pLeadInstr <|> pStrucDescEnd))
-- parses phrase annotations
pPhrase :: Parser [Annotation]
pPhrase = (list . End . Anno $ PhraseTrans) <$ pString " ->" `opt` []

-- parses a repeating phrase
pRepeat :: Parser [Annotation]
pRepeat = (list . End . Anno . Repeat) <$> (pString " x" *> pIntegerRaw) `opt` []

-- parses structural descriptions (strings after a ',' ) after a chord sequences
pStrucDescEnd :: Parser [Annotation]
pStrucDescEnd = (list . End . Anno) <$> pAnno

-- parses a three different kind of lead insturment descriptions: 
-- a start, an end, and a start and ending boundary
pLeadInstr :: Parser [Annotation]
pLeadInstr = pLeadInstrStart <|> pLeadInstrEnd <|> pLeadInstrStartEnd

-- start of a lead instrument description
pLeadInstrStart :: Parser [Annotation]
pLeadInstrStart = (list . Start . Instr) <$> (pSym '(' *> pInstr)

-- end of a lead instrument description
pLeadInstrEnd :: Parser [Annotation]
pLeadInstrEnd = (list . End . Instr) <$> pInstr  <* pSym ')'

-- start and end of a lead instrument description
pLeadInstrStartEnd :: Parser [Annotation]
pLeadInstrStartEnd = f <$> (pSym '(' *> pInstr <* pSym ')') 
  where f a = [Start $ Instr a, End $ Instr a]

pInstr :: Parser Instrument
pInstr = pInstrument <<|> pUnknownInstr
  
-- parses the different kind of lead instruments
pInstrument :: Parser Instrument
pInstrument =   Guitar         <$ pString "guitar"
            <|> Voice          <$ pString "vo" 
                               <* (pString "ice"    <|>  pString "cal")
            <|> Violin         <$ (pString "fiddle" <|>  
                                  (pString "violin" <* pMaybe (pSym 's')))
            <|> Banjo          <$ pString "banjo"
            <|> Synthesizer    <$ pString "synth" <* pMaybe (pString "esi" 
                               <*(pSym 's' <|> pSym 'z' ) <* pString "er")
            <|> Saxophone      <$ pString "saxophone"
            <|> Flute          <$ pString "flute"
            <|> Drums          <$ pString "drum" <* pMaybe (pString " kit" 
                                                        <|> pString "s")
            <|> SteelDrum      <$ pString "steel " <*  (pString "drum"
                               <|>  pString "pan") <* pMaybe (pSym 's')
            <|> Trumpet        <$ pString "trumpet" <* pMaybe (pSym 's')
            <|> Vibraphone     <$ pString "vibraphone"
            <|> Piano          <$ pString "piano"
            <|> Harmonica      <$ pString "harmonica"
            <|> Organ          <$ pString "organ"
            <|> Keyboard       <$ pString "keyboard"
            <|> Strings        <$ pString "strings"
            <|> Trombone       <$ pString "trombone"
            <|> Electricsitar  <$ pString "electric"<* pMabSpc <* pString "sitar"
            <|> Pennywhistle   <$ pString "pennywhistle"
            <|> Tenorsaxophone <$ pString "tenor" <* pMabSpc <* pString "saxophone"
            <|> Whistle        <$ pString "whistle"
            <|> Oboe           <$ pString "oboe"
            <|> Tambura        <$ pString "tambura"
            <|> Horns          <$ (pString "horns" <|> pString "brass")
            <|> Clarinet       <$ pString "clarinet"
            <|> Electricguitar <$ pString "electric"<* pMabSpc <* pString "guitar"
            <|> Steelguitar    <$ pString "steel"   <* pMabSpc <* pString "guitar"
            <|> Tenorhorn      <$ pString "tenor"   <* pMabSpc <* pString "horn"
            <|> Percussion     <$ pString "percussion"
            <|> Rhythmguitar   <$ pString "rhythm"  <* pMabSpc <* pString "guitar"
            <|> Hammondorgan   <$ pString "hammond" <* pMabSpc <* pString "organ"
            <|> Harpsichord    <$ pString "harpsichord"
            <|> Cello          <$ pString "cello"
            <|> Acousticguitar <$ pString "acoustic" <* pMabSpc <* pString "guitar"
            <|> Bassguitar     <$ pString "bass" <* pMaybe (pString " guitar")
            <|> Bongos         <$ pString "bongos"
            <|> Horn           <$ pString "horn"
            <|> Sitar          <$ pString "sitar"
            <|> Barisaxophone  <$ pString "baritone" <* pMabSpc <* pString "saxophone"
            <|> Accordion      <$ pString "accordion"
            <|> Tambourine     <$ pString "tambourine"
            <|> Kazoo          <$ pString "kazoo"

pUnknownInstr :: Parser Instrument
pUnknownInstr = UnknownInstr <$> pList1 pLower

-- a parser that recognises either a metre change or a modulation
pMetaChange :: Parser  (Either (Double, [BBChord]) Meta)
pMetaChange = pModulation <|> pMetreChange

-- recongises a modulation and returns the new tonic
pModulation :: Parser (Either (Double, [BBChord]) Meta)
pModulation = Right <$> (pMetaPrefix *> pKeyRoot)

-- recongises a metre change and returns the new time signature
pMetreChange :: Parser (Either (Double, [BBChord]) Meta)
pMetreChange = Right <$> (pMetaPrefix *> pMetre)

--------------------------------------------------------------------------------
-- Post-processing the chord sequence data
--------------------------------------------------------------------------------

-- Top-level parser for parsing chords sequence lines an annotations
pChordLinesPost :: TimeSig -> Parser [TimedData BBChord]
pChordLinesPost ts = (interp . setTiming) <$> pChordLines ts 

-- labels every line with the corresponding starting and ending times (where
-- the end time is actually the start time of the next chord line)
setTiming :: [(Double, a)] -> [TimedData a]
setTiming [ ] = []
setTiming [_] = [] -- remove the end 
setTiming (a : b : cs) = TimedData (snd a) [Time (fst a), Time (fst b)] 
                         : setTiming (b:cs)

-- interpolates the on- and offset for every 'BBChord' in a timestamped list  
-- of 'BBChord's 
interp :: [TimedData [BBChord]] -> [TimedData BBChord]
interp = concatMap interpolate . fixBothBeatDev where

  -- splits a 'TimedData [BBChord]' into multiple instances interpolating
  -- the off an onsets by evenly dividing the time for every beat.
  interpolate :: TimedData [BBChord] -> [TimedData BBChord]
  interpolate td = 
    let on  = onset td
        off = offset td
        dat = getData td
        bt  = (off - on) / genericLength dat
    in  zipWith3 timedData dat [on, (on+bt) ..] [(on+bt), (on+bt+bt) ..]

-- The beat deviation occurs both at the beginning and at the end of piece,
-- but the principle of correction is exactly the same (but mirrored). Hence
-- 'fixForward' and 'fixBackward' both rely on 'fixOddLongBeats'
fixForward, fixBackward, fixBothBeatDev :: [TimedData [BBChord]] 
                                        -> [TimedData [BBChord]]
fixBothBeatDev = fixBackward . fixForward
fixForward     = fixOddLongBeats Forward 
                 acceptableBeatDeviationMultiplier
fixBackward    = reverse . fixOddLongBeats Backward  -- = reversed fix Forward
                 acceptableBeatDeviationMultiplier . reverse

data Direction = Forward | Backward
    
-- We discovered that the 'Billboard.Tests.oddBeatLengthTest' failed at quite
-- some songs. The reason of failure is often caused by the /silence/ 
-- timestamps at the beginning and end of a song. Probably, the annotators 
-- marked /silence/ when there was really not much to hear any more. 
-- This, however, is a problem: apparently there is a discrepancy between the 
-- last musical beat of a song and the actual silence that clearly distorts 
-- the 'interp'olation of the last line of annotated chords. Hence, we 
-- use a different kind of interpolation for this last line of chords. We use 
-- the average beat length of the previous line to predict the beat durations 
-- of the chords and fill the /gap/ between the last chord and the /silence/
-- annotation with additional 'N' chords.
fixOddLongBeats :: Direction -> Double -> [TimedData [BBChord]] 
                -> [TimedData [BBChord]]
fixOddLongBeats dir beatDev song = sil ++ (fixOddLongLine . markStartEnd dir $ cs)  where

  -- separate the lines containing silence N chords at the beginning
  -- from the lines that contain musical chords
  (sil,cs) = break (not . and . map (isNoneBBChord) . getData) song
  -- precalculate the average beat length, filtering lines that contain 
  -- none harmonic data (in the from of N chords)
  avgBt = avgBeatLens . filter (and . map (not . isNoneBBChord) . getData ) $ cs        
  
  -- Marks the start and beginning of the chords sequence
  markStartEnd :: Direction -> [TimedData [BBChord]] -> [TimedData [BBChord]]
  markStartEnd _        []         = []
  markStartEnd Forward  (fc : rst) = fmap markStart fc : rst
  markStartEnd Backward (fc : rst) = fmap markEnd  fc : rst  
  
  -- does the actual marking
  markStart, markEnd :: [BBChord] -> [BBChord]
  markStart  [] = []
  markStart (h:t) = addStart (Anno Chords) h : t
  markEnd    [] = []
  markEnd l = let (lst : rst) = reverse l 
              in reverse (addEnd (Anno Chords) lst : rst)
  
  -- Fixes the distorted interpolation
  fixOddLongLine :: [TimedData [BBChord]] -> [TimedData [BBChord]]
  fixOddLongLine (l : n : ls ) = 
    case (avgBeatLen l >= ((1 + beatDev) * avgBt), dir) of
      (True, Forward ) -> fmap (replicateNone (avgBeatLen n) l ++) l : n : ls
      (True, Backward) -> fmap (++ replicateNone (avgBeatLen n) l) l : n : ls
      (False, _      ) ->                                          l : n : ls
  fixOddLongLine l             = l
  
  -- fills the "gap" with none chords
  replicateNone :: Double -> TimedData [BBChord] -> [BBChord]
  replicateNone prvBeat d = 
    -- calculate the number of beats expected, minus the chords in the list
    let nrN  = (round ((offset d - onset d) / prvBeat)) - (length . getData $ d) 
        repN = noneBBChord {weight = Beat}
    -- annotate that this is an interpolated N list
    in addLabel (Anno InterpolationInsert) 
                (noneBBChord : replicate (pred nrN) repN)
  
  -- Calculates the average length of a beat in a list of Timed BBChords
  avgBeatLens :: [TimedData [BBChord]] -> Double
  avgBeatLens l = (sum . map avgBeatLen $ l) / genericLength l 
  
  -- Calculates the average length of a beat
  avgBeatLen :: TimedData [BBChord] -> Double
  avgBeatLen td = (offset td - onset td) / genericLength (getData td)   
    
--------------------------------------------------------------------------------
-- Chord sequence data parsers
--------------------------------------------------------------------------------

-- Parses the /musical part/ of the Billboard data file by recursively parsing
-- the lines of chords, silence, metre changes or modulations
pChordLines :: TimeSig -> Parser [(Double, [BBChord])]
pChordLines ts = do p <- pLine ts  -- parse one line
                    r <- case p of
                          -- if we parse chords, continue
                          (Left  t            ) -> concatLines t
                          -- if we encounter a new global metre we use this 
                          -- metre for the parsing the following chords
                          (Right (Metre newTs)) -> pChordLines newTs 
                          -- we ignore modulations for now
                          (Right _            ) -> pChordLines ts
                    return r where
  
  -- concatenates the parsed lines of chords recursively
  concatLines :: (Double, [BBChord]) -> Parser [(Double, [BBChord])]
  concatLines t = case isEnd . head . snd $ t of
                    True  -> (t :) <$> pure []        -- stop condition
                    False -> (t :) <$> pChordLines ts -- continue

-- Parses one line which can be chords, meta information, or end/silence
pLine :: TimeSig -> Parser (Either (Double, [BBChord]) Meta)         
pLine ts = (pChordLine ts <|> pSilenceLine <|> pMetaChange) <* pLineEnd

-- parses a line annotated with "silence"
pSilenceLine :: Parser (Either (Double, [BBChord]) Meta) 
pSilenceLine = f <$> pDoubleRaw <* pSym '\t' <*> (pZSilence  <|> pSongEnd)
  where f a b = Left (a,[b])
  
-- parses a line with annotated chord sequence data
pChordLine :: TimeSig -> Parser (Either (Double, [BBChord]) Meta)
pChordLine ts = Left <$> (setAnnotations <$> pDoubleRaw <* pSym '\t'
                                        <*> pStructStart
                                        <*> pChordSeq ts
                                        <*> pEndAnnotations)
      
-- merges the different forms information (annotations, beats, chords, time-
-- stamps) into a timestamped list of 'BBChord's 
setAnnotations :: Double -> [Annotation] -> [BBChord] -> [Annotation] 
               -> (Double, [BBChord])
setAnnotations d _   [ ]    _   = (d, []) -- no chords, just a timestamp
setAnnotations d srt chords end = 
  (d, updateLast (addAnnotation end'') (addAnnotation srt' c : cs))

  where -- put the 'starting annotations' at the beginning
        (srt', end') = first (++ srt) (partition isStart end)
        -- if there exists annotated repeats the chords sequence in 'chords'
        -- is repeated, if not 'chords' is returned.
        (c : cs, end'') = case partition isRepeat end' of
             -- in the case that we find a repetition we return an updated
             -- list of annotations (rep'') that does not contain the repetition
             -- Afterall, it has been expanded and could only confuse users
             ([r], nr) -> (concat $ replicate (getRepeats r) chords, nr)
             ([ ], _ ) -> (chords, end')
             _   -> error "Billboard.Billboardparser: multiple repeats found!" 
  
        -- replaces the list of annotations in a BBChord
        addAnnotation :: [Annotation] -> BBChord -> BBChord
        addAnnotation ans crd = crd {annotations = ans}
        
        -- applies a function to the last element of a list
        updateLast :: (a -> a) -> [a] -> [a]
        updateLast _ [ ]    = []
        updateLast f [x]    = [f x]
        updateLast f (x:xs) = x : updateLast f xs

-- Recognises the "Z" character, and several silence annotations. If silence 
-- (see pSilence) is explicitly annotated the annotation is also stored in the
-- outputed N chord label.   
pZSilence :: Parser BBChord  
pZSilence = endChord <$> 
      ((pString "Z" <* pPrimes) *> pMaybe (pString ", " *> pSilence)
      <|> Just <$> pSilence) where
  -- if there are silence annotations add them to a N chord
  endChord :: Maybe Label -> BBChord
  endChord = maybe noneC ((flip addStartEnd) noneC)  
  
  noneC = addStart (Struct 'Z' 0) noneBBChord
  
  -- recognises a "silence" annotation, no chords are sounding
  pSilence :: Parser Label
  pSilence = Anno <$> (Silence    <$ pString "silence" 
                  <|>  Noise      <$ pString "noise"
                  <|>  Applause   <$ pString "applause"
                  <|>  TalkingEnd <$ pString "talking" 
                  <|>  Fadeout    <$ pString "fadeout" 
                  <|>  PreIntro   <$ pString "pre" <* pMabSpcDsh  
                                                   <* pString "intro")
                                                    
-- parses the end of a song
pSongEnd :: Parser BBChord
pSongEnd = (flip addEnd) noneBBChord <$> ((Anno SongEnd) <$ pString "end")

-- recognises a chord sequence like: 
-- "| Db:maj Gb:maj/5 | Ab:maj Db:maj/5 | Bb:min Bb:min/b7 Gb:maj . | Ab:maj |\"
pChordSeq :: TimeSig -> Parser [BBChord]
pChordSeq ts = setWeight . concat <$> (pSym '|' 
                                   *> pList1Sep_ng (pString "|") (pBar ts) 
                                  <*  pSym '|') where
  -- Log the start of a line (which generally marks the start of a phrase) as
  -- a "metrical weight", which is used in printing the sequence to the user.
  setWeight :: [BBChord] -> [BBChord]
  setWeight []    = []
  setWeight (h:t) = h {weight = LineStart} : t

-- In a bar we can encounter chords and optionally we can also encounter an
-- additional time signature description
pBar :: TimeSig -> Parser [BBChord]
pBar ts = markBarStart <$> (updateRep <$> (pSym ' ' 
                                       *> (pParens pTimeSig `opt` ts))
                                      <*> pBarChords)

-- parses the chords within one bar
pBarChords :: Parser [BBChord] 
pBarChords = pList1Sep_ng (pSym ' ') (pBBChord <|> pRepChord) <* pSym ' '

-- marks the start of a bar by setting the beat weight field to 'Bar'
markBarStart :: [BBChord] -> [BBChord]
markBarStart []    = []
markBarStart (h:t) = h {weight = Bar} : t
  
-- within a bar there can be one chord, two chords (representing the sounding
-- chord in the first and second halve of the bar), multiple chords, and 
-- repetitions marked with a '.'
updateRep :: TimeSig -> [BBChord] -> [BBChord]
updateRep _  [ ]      = error "updateRep: no chords to update"
updateRep ts [c]      = replChord (tatumsPerBar ts) c          -- one chord
updateRep ts [c1, c2] = let t = (tatumsPerBar ts) `div` 2      -- two chords
                        in  replChord t c1 ++ replChord t c2    
updateRep ts cs       = update cs                             -- multiple chords
  -- updates the repetited chords ('.')
  where update :: [BBChord] -> [BBChord]
        update [ ]    = [ ]
        update [x]    = replChord (chordsPerDot ts) x
        update (x:y:xs) = case weight y of
          -- Because at parsing time we do not know which chord is repeated 
          -- by a '.' we replace the chord in the 'BBChord' by the previous 
          -- chord (at parsing time a None chord is stored in the BBchord)
          Beat   -> replChord (chordsPerDot ts) x ++ update (y {chord = chord x}: xs)
          Change -> replChord (chordsPerDot ts) x ++ update (y : xs)
          _      -> error "update: unexpected beat weight" -- cannot happen

-- replicates an 'BBChord' the first chord will have a 'Change'
-- weigth and the remaining chords will have a 'Beat' weight
replChord :: Int -> BBChord -> [BBChord]
replChord d c = c : replicate (pred d) c {weight = Beat}

-- parses a chord a repetition symbol '.'
pRepChord :: Parser BBChord
pRepChord = noneBBChord {weight = Beat} <$ pSym '.'

-- parses a single chord into an 'BBChord'
pBBChord :: Parser BBChord
pBBChord = BBChord [] Change <$> pChord


--------------------------------------------------------------------------------
-- general parsers combinators
--------------------------------------------------------------------------------

-- optionally parsers a space ' '
pMabSpc:: Parser (Maybe Char)
pMabSpc = pMaybe (pSym ' ')

-- optionally parsers a space ' ' or a dash '-'
pMabSpcDsh :: Parser (Maybe Char)
pMabSpcDsh = pMaybe (pSym ' ' <|> pSym '-')

pOptWrapPar :: String -> Parser String
pOptWrapPar s = pSym '(' *> pString s <* pSym ')' <|> pString s

-- accepts any string
pReadableStr :: Parser String
pReadableStr = pList1 pReadableSym

-- accepts a 'Char' in the range of:  
-- !\"#$%&'()  
-- ,-./0123456789:;
-- ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz
pReadableSym :: Parser Char
pReadableSym = pRange (' ', ')') <|> pRange(',',';') <|> pRange ('?', 'z')

-- parses a line ending
pLineEnd :: Parser Char
pLineEnd = pMany (pSym ' ') *> (pSym '\n' <|> pSym '\r')

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

tuple :: a -> b -> (a,b)
tuple a b = (a,b)

list :: a -> [a]
list a = [a]

