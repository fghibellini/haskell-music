{-# LANGUAGE BlockArguments #-}

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

over :: [Float] -> [Float] -> [Float]
over = zipWith (+)

after :: [Float] -> [Float] -> [Float]
after a b = a <> b

midiToFreq :: Float -> Float
midiToFreq n = 440.0 * (2 ** (1.0 / 12.0)) ** n

applyEnvelope :: [Float] -> [Float] -> [Float]
applyEnvelope = zipWith (*)

linearly :: Int -> Float -> Float -> [Float]
linearly sampleCount fromVal toVal = (\i -> fromVal + (toVal - fromVal) * (fromIntegral i / fromIntegral sampleCount)) <$> [0..sampleCount]

-- (1/10)s attack
-- (6/10)s sustain
-- (9/10)s decay
keyPressEnvelope :: [Float]
keyPressEnvelope = linearly (sps `div` 10) 0 1 `after` linearly (6 * (sps `div` 10)) 1 1 `after` linearly (3 * (sps `div` 10)) 1 0

wave :: [Float]
wave = applyEnvelope keyPressEnvelope (sinew (midiToFreq 0)) `after` applyEnvelope keyPressEnvelope (sinew (midiToFreq 1) `over` sinew (midiToFreq 2))
  where
    sinew :: Float -> [Float]
    sinew freq =
      let
        tvals = fromIntegral <$> [0 .. sps]
        f sample_n = 0.2 * sin (2 * pi * sample_n * freq / fromIntegral sps)
      in
        map f tvals


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
      sample_count = length wave
      chunkSizeBytes = sample_count * bytes_per_sample * num_channels
    buffer <- mallocBytes chunkSizeBytes
    traverse (\(i, x) -> writeInt16OffPtr (castPtr buffer) (2*i + 1) x) (take (2 * sps) $ zip [0..] $ (floatToInt16 <$> wave))
    chunkMem <- malloc
    poke chunkMem $ RawMixer.Chunk
      (fromIntegral 1)
      buffer
      (fromIntegral chunkSizeBytes)
      (fromIntegral 128)
    play $ Chunk chunkMem
    putStrLn "played!"
    delay 3000
  quit

-- save :: IO ()
-- save = withFile "./output.bin" WriteMode $ flip BS.hPutBuilder $ fold (BS.floatLE <$> wave)
-- 
-- play :: IO ()
-- play = save >> system "ffplay -showmode 1 -f f32le -ar 48000 output.bin" >> pure ()



