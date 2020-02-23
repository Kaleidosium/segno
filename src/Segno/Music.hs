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
  try (string "end")
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
        + (case a of Just 'l' -> -1; Just 'h' -> 1; Nothing -> 0)
      f =  case h of Just '-' ->  2; Just '_' -> 4; Nothing -> 1
  return [(0, NoteOn c m 80), (f*24, NoteOff c m 0)]

notes :: TrackNumber -> String -> Either ParseError [(Ticks, Message)]
notes c = parse (element c) ""
