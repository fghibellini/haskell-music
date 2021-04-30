{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Lib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import System.IO (withFile, IOMode(WriteMode))
import Data.Foldable (fold)
import System.Process (system)
import SDL.Init (initialize, InitFlag(InitAudio), quit)
import SDL.Mixer (Chunk(Chunk), withAudio, Audio(..), Format(FormatS16_LSB), Output(Mono, Stereo), chunkDecoders, play)
import qualified SDL.Raw.Mixer as RawMixer
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (malloc, mallocBytes)
import Foreign.Storable (poke)
import Data.Int (Int16)
import GHC.Storable (writeInt16OffPtr)
import SDL.Time (delay)

import Control.Concurrent (threadDelay)

-- samples per second
sps :: Int
sps = 48000


newtype Stream a = Stream { unStream :: [a] } deriving Functor

-- common audio stream types
type AStream = Stream Float
type DStream = Stream Int16

type Envelope = Stream SFactor

newtype SFactor = SFactor { unFactor :: Float }

-- Data that can be in an audio stream
-- we can't use standard haskell classes since we want
-- to be able to run floating-point operations (e.g. sin) on
-- integer types as well
class (Num a) => AudioNum a where
  scale :: SFactor -> a -> a
  aDiv :: a -> a -> a
  minVal :: a
  maxVal :: a
  fromInt :: Integral b => b -> a

instance AudioNum Float where
  scale (SFactor f) x = f * x
  minVal = -1.0
  maxVal = 1.0
  fromInt = fromIntegral
  aDiv = (/)

over :: AudioNum a => Stream a -> Stream a -> Stream a
over (Stream as) (Stream bs) = Stream (zipWith (+) as bs)

andThen :: AudioNum a => Stream a -> Stream a -> Stream a
andThen (Stream as) (Stream bs) = Stream (as <> bs)

midiToFreq :: Float -> Float
midiToFreq n = 440.0 * (2 ** (1.0 / 12.0)) ** n

applyEnvelope :: AudioNum a => Envelope -> Stream a -> Stream a
applyEnvelope (Stream es) (Stream xs) = Stream $ zipWith (\e x -> e `scale` x) es xs


-- (1/10)s attack
-- (6/10)s sustain
-- (9/10)s decay
keyPressEnvelope :: Envelope
keyPressEnvelope = SFactor <$> es
  where
    es = linearly (sps `div` 10) 0 1 `andThen` linearly (6 * (sps `div` 10)) 1 1 `andThen` linearly (3 * (sps `div` 10)) 1 0

sinew :: Float -> Stream Float
sinew freq =
  let
    tvals = fromIntegral <$> [0 .. sps]
    f sample_n = 0.2 * sin (2 * pi * sample_n * freq / fromIntegral sps)
  in
    Stream $ map f tvals

wave :: Stream Float
wave = applyEnvelope keyPressEnvelope (sinew (midiToFreq 0)) `andThen` applyEnvelope keyPressEnvelope (sinew (midiToFreq 1) `over` sinew (midiToFreq 2))


-- chunks per second
cps :: Int
cps = 60

floatToInt16 :: Float -> Int16
floatToInt16 x = floor $ x * (fromIntegral (maxBound :: Int16))

main :: IO ()
main = do
  () <- initialize [InitAudio]
  let
    audio = Audio
      { audioFrequency = sps
      , audioFormat = FormatS16_LSB
      , audioOutput = Stereo
      }
  withAudio audio (sps `div` cps) do
    let
      num_channels = 2 -- left and right
      bytes_per_sample = 2 -- 16bit numbers
      sample_count = let Stream w = wave in length w
      chunkSizeBytes = sample_count * bytes_per_sample * num_channels
    buffer <- mallocBytes chunkSizeBytes
    traverse (\(i, x) -> writeInt16OffPtr (castPtr buffer) (2*i + 1) x) (take (2 * sps) $ zip [0..] $ (floatToInt16 <$> unStream wave))
    chunkMem <- malloc
    poke chunkMem $ RawMixer.Chunk
      (fromIntegral 1)
      buffer
      (fromIntegral chunkSizeBytes)
      (fromIntegral 128)
    play $ Chunk chunkMem
    delay $ ((fromIntegral sample_count * 1000) `div` fromIntegral sps) + 100 -- extra 100ms so it doesn't cut-off
  quit

-- save :: IO ()
-- save = withFile "./output.bin" WriteMode $ flip BS.hPutBuilder $ fold (BS.floatLE <$> wave)
-- 
-- play :: IO ()
-- play = save >> system "ffplay -showmode 1 -f f32le -ar 48000 output.bin" >> pure ()



