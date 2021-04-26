{-# LANGUAGE BlockArguments #-}

module Main where

import Lib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import System.IO (withFile, IOMode(WriteMode))
import Data.Foldable (fold)
import System.Process (system)
import SDL.Init (initialize, InitFlag(InitAudio), quit)
import SDL.Mixer (Chunk(Chunk), withAudio, Audio(..), Format(FormatS16_LSB), Output(Mono), chunkDecoders, play)
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

-- chunks per second
cps :: Int
cps = 60

over :: [Float] -> [Float] -> [Float]
over = zipWith (+)

after :: [Float] -> [Float] -> [Float]
after a b = a <> b

midiToFreq :: Float -> Float
midiToFreq n = 440.0 * (2 ** (1.0 / 12.0)) ** n

--applyEnvelope :: [Float] -> [Float] -> [Float]
--applyEnvelope = zipWith (*)
--
--keyPressEnvelope :: [Float]
--keyPressEnvelope = 0.1
--      let
--        tvals = fromIntegral <$> [0 .. sps]
--        f sample_n = sin (sample_n / fromIntegral sps)
--      in
--        map f tvals

wave :: [Float]
wave = sinew (midiToFreq 0) `after` (sinew (midiToFreq 1) `over` sinew (midiToFreq 2))
  where
    sinew :: Float -> [Float]
    sinew freq =
      let
        tvals = fromIntegral <$> [0 .. sps]
        f sample_n = 0.2 * sin (2 * pi * sample_n * freq / fromIntegral sps)
      in
        map f tvals


floatToInt16 :: Float -> Int16
floatToInt16 x = floor $ x * (fromIntegral (maxBound :: Int16))

main :: IO ()
main = do
  () <- initialize [InitAudio]
  let
    audio = Audio
      { audioFrequency = sps
      , audioFormat = FormatS16_LSB
      , audioOutput = Mono
      }
  withAudio audio (sps `div` cps) do
    let chunkSize = 2 * sps * 2
    buffer <- mallocBytes chunkSize
    traverse (\(i, x) -> writeInt16OffPtr (castPtr buffer) i x) (take (2 * sps) $ zip [0..] $ (floatToInt16 <$> wave))
    chunkMem <- malloc
    poke chunkMem $ RawMixer.Chunk
      (fromIntegral 1)
      buffer
      (fromIntegral chunkSize)
      (fromIntegral 128)
    play $ Chunk chunkMem
    putStrLn "played!"
    delay 2000
  quit
    
  

-- save :: IO ()
-- save = withFile "./output.bin" WriteMode $ flip BS.hPutBuilder $ fold (BS.floatLE <$> wave)
-- 
-- play :: IO ()
-- play = save >> system "ffplay -showmode 1 -f f32le -ar 48000 output.bin" >> pure ()



