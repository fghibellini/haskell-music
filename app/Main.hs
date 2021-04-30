
module Main where

import Zwk.Core
import Zwk.Envelope

wave :: Stream Float
wave = toneA `andThen` (toneB `over` toneC)
  where
    toneA = applyEnvelope keyPressEnvelope (sinew (midiToFreq 0))
    toneB = applyEnvelope keyPressEnvelope (sinew (midiToFreq 1))
    toneC = sinew (midiToFreq 2)

main :: IO ()
main = do
  play $ analogToDigital wave

