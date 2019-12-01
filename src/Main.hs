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

module Main where

import Codec.Midi
import Control.Monad
import Data.List.Split
import Data.Typeable
import Prelude
import Segno.Music
import System.IO

data GMain =
  GMain { tokens :: [[String]] }
  deriving (Show, Eq)

tup f1 f2 f3 =
  foldr
    (\t trs ->
       case t of
         (a:[])     -> f1 a trs
         (a:b:[])   -> f2 a b trs
         (a:b:c:[]) -> f3 a b c trs
         _          -> "?" : trs)
    []

one = tup (\a trs -> a : trs) (\a _ trs -> a : trs) (\a _ _ trs -> a : trs)

two = tup (\_ trs -> trs) (\_ b trs -> b : trs) (\_ b _ trs -> b : trs)

thr = tup (\_ trs -> trs) (\_ _ trs -> trs) (\_ _ c trs -> c : trs)

codecMulti n =
  Midi {fileType = MultiTrack, timeDiv = TicksPerBeat 24, Codec.Midi.tracks = n}

looping (GMain y) = do
  allX@(x:xs) <- getLine
  case x of
    'q' -> do
      let trs@(tr1, tr2, tr3) = (one y, two y, thr y)
      putStr "Channel 1> "
      print tr1
      putStr "Channel 2> "
      print tr2
      putStr "Channel 3> "
      print tr3
      let m1 = (foldr (\x a -> (notes 1 x) ++ a) [] tr1)
      let m2 = (foldr (\x a -> (notes 2 x) ++ a) [] tr2)
      let m3 = (foldr (\x a -> (notes 3 x) ++ a) [] tr3)
      exportFile "output.mid" (codecMulti [m1, m2, m3])
    _ -> readLine y allX

readLine y x = do
  let f = filter (/= "") (splitOn " " x)
  looping (GMain (y ++ [f]))

main :: IO ()
main = looping (GMain [])
