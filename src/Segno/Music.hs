module Segno.Music where

import Codec.Midi
import Prelude

notes :: Int -> String -> [(Ticks, Message)]
notes c ('>':s) = [(0, Copyright s)]
notes c "pause" = [(0, NoteOn c 60 0), (24, NoteOff c 60 0)]
notes c ('#':s) = [(0, Text s)]
notes c "end" = [(0, TrackEnd)]
notes c "fin" = [(0, TrackEnd)]
notes c (a:l:o:d:[]) =
  case d of
    '-' -> fmap halfNote (notes c (a : l : o : []))
    '_' -> fmap (halfNote . halfNote) (notes c (a : l : o : []))
    _   -> notes c (a : l : o : [])
notes c (x:y:z:[]) =
  case z of
    '-' -> fmap halfNote (notes c (x : y : []))
    '_' -> fmap (halfNote . halfNote) (notes c (x : y : []))
    _ ->
      case x of
        'l' -> fmap lowNote (notes c (y : z : []))
        'h' -> fmap highNote (notes c (y : z : []))
        _   -> notes c (y : z : [])
notes c (l:o:[]) =
  let m =
        case o of
          '0' -> 48
          '1' -> 60
          '2' -> 72
          '3' -> 84
          '4' -> 96
          _   -> 60
   in case l of
        'c' -> [(0, NoteOn c m 80), (24, NoteOff c m 0)]
        'd' -> [(0, NoteOn c (m + 2) 80), (24, NoteOff c (m + 2) 0)]
        'e' -> [(0, NoteOn c (m + 4) 80), (24, NoteOff c (m + 4) 0)]
        'f' -> [(0, NoteOn c (m + 5) 80), (24, NoteOff c (m + 5) 0)]
        'g' -> [(0, NoteOn c (m + 7) 80), (24, NoteOff c (m + 7) 0)]
        'a' -> [(0, NoteOn c (m + 9) 80), (24, NoteOff c (m + 9) 0)]
        'b' -> [(0, NoteOn c (m + 11) 80), (24, NoteOff c (m + 11) 0)]
        _   -> [(0, NoteOn c m 80), (24, NoteOff c m 0)]
notes c s = [(0, Text s)]

lowNote :: (Ticks, Message) -> (Ticks, Message)
lowNote (d, NoteOn x m v)  = (d, NoteOn x (m - 1) v)
lowNote (d, NoteOff x m v) = (d, NoteOff x (m - 1) v)
lowNote m                  = m

highNote :: (Ticks, Message) -> (Ticks, Message)
highNote (d, NoteOn x m v)  = (d, NoteOn x (m + 1) v)
highNote (d, NoteOff x m v) = (d, NoteOff x (m + 1) v)
highNote m                  = m

halfNote :: (Ticks, Message) -> (Ticks, Message)
halfNote (d, NoteOn x m v)  = (d, NoteOn x m v)
halfNote (d, NoteOff x m v) = (d + d, NoteOff x m v)
halfNote m                  = m
