-- Dania Rifki Presents:
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

data ChannelData = Channel Int String
    deriving (Show, Eq)

codecMulti :: [Track Ticks] -> Midi
codecMulti n =
  Midi {fileType = MultiTrack, timeDiv = TicksPerBeat 24, tracks = n}

dumpToFile :: [ChannelData] -> IO ()
dumpToFile inputs = do
  forM_ inputs $ \(Channel track input) -> do
    putStr $ "Channel " ++ show track ++ "> "
    print input
  let tracks = either (error . show) concat $ forM inputs $ \(Channel track input) -> notes track input
  exportFile "output.mid" (codecMulti [tracks])

parseInput :: [String] -> [ChannelData]
parseInput = go 0
    where
        go _ [] = []
        go i (x:xs) = Channel i x : go (i+1) xs

looping =
  getLine >>= \case
    'q':_ -> return []
    allX -> do
      let f = parseInput $ words allX
      fs <- looping
      return $ f ++ fs

main :: IO ()
main = looping >>= dumpToFile
