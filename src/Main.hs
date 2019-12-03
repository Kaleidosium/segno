-- Rifki K. Presents:
--
--     .;cc:lxxdoc'.
--   'dXNx;oNMMMMMX0c        .:c'
--  '0WMO'.xMMMMMMMMx.     .cXWk'
--  oWMMXc 'kNMMMNkl'     'kWKc.
--  dMMMMNx,.':::'      .cKWk'
--  :XMMMMMNOl'        'xNXc.';'
--   lNMMMMMMMNOl,.   :KWk'.xNMNd.
--    cKMMMMMMMMMN0xdkNXl. .xWMWx.
--     'dXMMMMMMMMMMMMM0;.  .,;,.
--       'dXWMMMMMMMMMMMW0o'
--         .ckXMMMMMMMMMMMMXd'
--   ;xOx;   'OMNNWMMMMMMMMMMXo.
--  .OMMM0' ;0WO;.:oxXWMMMMMMMWO'
--   ,xkx;'dNXo.     .:xKWMMMMMMO.
--       ;0WO;          .:xNMMMMWl
--     .dNXo.     ;ldO0Od;.,OWMMMd
--    ;0W0;      lNMMMMMMNl '0MMNc
--   ;kOo.      .dMMMMMMMMd.;KMXl.
--               .dO0XNWWKdlxkl'
--                  .,:cc::,.
--

{-# LANGUAGE LambdaCase #-}

module Main where

import Codec.Midi
import Control.Monad
import Data.Maybe
import Data.List.Split
import Data.Typeable
import Prelude
import Segno.Music
import System.IO
import Text.Parsec

data ChannelData = Channel1 String | Channel2 String | Channel3 String
    deriving (Show, Eq)

codecMulti :: [Track Ticks] -> Midi
codecMulti n =
  Midi {fileType = MultiTrack, timeDiv = TicksPerBeat 24, tracks = n}

dumpToFile :: [ChannelData] -> IO ()
dumpToFile input = do
  let tr1 = [ a | Channel1 a <- input ]
  let tr2 = [ a | Channel2 a <- input ]
  let tr3 = [ a | Channel3 a <- input ]
  putStr "Channel 1> "
  print tr1
  putStr "Channel 2> "
  print tr2
  putStr "Channel 3> "
  print tr3
  let m1 = either (error "Parse Error!" . show) concat $ mapM (notes 1) tr1
  let m2 = either (error "Parse Error!" . show) concat $ mapM (notes 2) tr2
  let m3 = either (error "Parse Error!" . show) concat $ mapM (notes 3) tr3
  exportFile "output.mid" (codecMulti [m1, m2, m3])

parseInput :: [String] -> IO (Maybe [ChannelData])
parseInput [a]       = return $ Just [Channel1 a]
parseInput [a, b]    = return $ Just [Channel1 a, Channel2 b]
parseInput [a, b, c] = return $ Just [Channel1 a, Channel2 b, Channel3 c]
parseInput _         = error "Error: Unrecognized input!" >> return Nothing

looping =
  getLine >>= \case
    'q':_ -> return []
    allX -> do
      f <- parseInput $ words allX
      fs <- looping
      return $ concat f ++ fs

main :: IO ()
main = looping >>= dumpToFile

