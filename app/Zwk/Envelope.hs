
module Zwk.Envelope 
  ( linearly
  , keyPressEnvelope
  ) where

import Zwk.Core

linearly :: AudioNum a => Int -> a -> a -> Stream a
linearly sampleCount fromVal toVal =
  Stream $ ((\i -> fromVal + (toVal - fromVal) * (fromInt i `aDiv` fromInt sampleCount)) <$> [0..sampleCount])

-- (1/10)s attack
-- (6/10)s sustain
-- (9/10)s decay
keyPressEnvelope :: Envelope
keyPressEnvelope = SFactor <$> es
  where
    es = linearly (sps `div` 10) 0 1 `andThen` linearly (6 * (sps `div` 10)) 1 1 `andThen` linearly (3 * (sps `div` 10)) 1 0

