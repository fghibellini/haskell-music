{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}

module Zwk.Core where

import Lib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import System.IO (withFile, IOMode(WriteMode))
import Data.Foldable (fold)
import System.Process (system)
import SDL.Init (initialize, InitFlag(InitAudio), quit)
import SDL.Mixer (Chunk(Chunk), withAudio, Audio(..), Format(FormatS16_LSB), Output(Mono, Stereo), chunkDecoders, play)
import qualified SDL.Mixer as Mixer
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

sinew :: Float -> Stream Float
sinew freq =
  let
    tvals = fromIntegral <$> [0 .. sps]
    f sample_n = 0.2 * sin (2 * pi * sample_n * freq / fromIntegral sps)
  in
    Stream $ map f tvals

-- chunks per second
cps :: Int
cps = 60

floatToInt16 :: Float -> Int16
floatToInt16 x = floor $ x * (fromIntegral (maxBound :: Int16))

analogToDigital :: AStream -> DStream
analogToDigital = fmap floatToInt16

play :: DStream -> IO ()
play wave = do
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
    traverse (\(i, x) -> writeInt16OffPtr (castPtr buffer) (2*i + 1) x) (take (2 * sps) $ zip [0..] $ unStream wave)
    chunkMem <- malloc
    poke chunkMem $ RawMixer.Chunk
      (fromIntegral 1)
      buffer
      (fromIntegral chunkSizeBytes)
      (fromIntegral 128)
    Mixer.play $ Chunk chunkMem
    delay $ ((fromIntegral sample_count * 1000) `div` fromIntegral sps) + 100 -- extra 100ms so it doesn't cut-off
  quit

-- save :: IO ()
-- save = withFile "./output.bin" WriteMode $ flip BS.hPutBuilder $ fold (BS.floatLE <$> wave)
-- 
-- play :: IO ()
-- play = save >> system "ffplay -showmode 1 -f f32le -ar 48000 output.bin" >> pure ()



