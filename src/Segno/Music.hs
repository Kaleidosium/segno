{-# LANGUAGE FlexibleContexts #-}

module Segno.Music where

import Codec.Midi
import Text.Parsec
import Control.Applicative
import Prelude

type TrackNumber = Int

element c = choice [ copyright
                   , pause c
                   , end
                   , note c
                   , text
                   ]

copyright = do
  char '>'
  s <- anyChar `manyTill` newline
  return [(0, Copyright s)]

pause c = do
  string "pause"
  return [(0, NoteOn c 60 0), (24, NoteOff c 60 0)]

end = do
  string "end"
  return [(0, TrackEnd)]

text = do
  Text.Parsec.optional $ char '#'
  s <- anyChar `manyTill` newline
  return [(0, Text s)]

note c = do
  a <- optionMaybe $ oneOf "lh"
  l <- oneOf "cdefgab"
  o <- oneOf "01234"
  h <- optionMaybe $ oneOf "-_"
  let m = (case o of '0' -> 48; '1' -> 60; '2' -> 72; '3' -> 84; '4' -> 96; _ -> 60)
        + (case l of 'c' -> 0; 'd' -> 2; 'e' -> 4; 'f' -> 5; 'g' -> 7; 'a' -> 9; 'b' -> 11; _ -> 0)
  let f1 = case a of
            Just 'l' -> lowNote
            Just 'h' -> highNote
            Nothing  -> id
  let f2 = case h of
            Just '-' -> halfNote
            Just '_' -> halfNote . halfNote
            Nothing  -> id
  return $ f1 . f2 <$> [(0, NoteOn c m 80), (24, NoteOff c m 0)]

lowNote :: (Ticks, Message) -> (Ticks, Message)
lowNote (d, NoteOn x m v)  = (d, NoteOn x (m - 1) v)
lowNote (d, NoteOff x m v) = (d, NoteOff x (m - 1) v)
lowNote m                  = m

highNote :: (Ticks, Message) -> (Ticks, Message)
highNote (d, NoteOn x m v)  = (d, NoteOn x (m + 1) v)
highNote (d, NoteOff x m v) = (d, NoteOff x (m + 1) v)
highNote m                  = m

halfNote :: (Ticks, Message) -> (Ticks, Message)
halfNote (d, NoteOff x m v) = (d + d, NoteOff x m v)
halfNote m                  = m

notes :: TrackNumber -> String -> Either ParseError [(Ticks, Message)]
notes c s = parse (element c) "" s