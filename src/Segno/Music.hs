module Segno.Music where

import Codec.Midi
import Prelude

type TrackNumber = Int

notes :: TrackNumber -> String -> [(Ticks, Message)]
notes c ('>':s) = [(0, Copyright s)]
notes c "pause" = [(0, NoteOn c 60 0), (24, NoteOff c 60 0)]
notes c ('#':s) = [(0, Text s)]
notes c "end" = [(0, TrackEnd)]
notes c ('l':s) = fmap lowNote $ notes c s
notes c ('h':s) = fmap highNote $ notes c s
notes c [l, o, '-'] = fmap halfNote $ notes c [l, o]
notes c [l, o, '_'] = fmap (halfNote . halfNote) $ notes c [l, o]
notes c [l, o] =
  let m = (case o of '0' -> 48; '1' -> 60; '2' -> 72; '3' -> 84; '4' -> 96; _ -> 60)
        + (case l of 'c' -> 0; 'd' -> 2; 'e' -> 4; 'f' -> 5; 'g' -> 7; 'a' -> 9; 'b' -> 11; _ -> 0)
  in [(0, NoteOn c m 80), (24, NoteOff c m 0)]
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
halfNote (d, NoteOff x m v) = (d + d, NoteOff x m v)
halfNote m                  = m
