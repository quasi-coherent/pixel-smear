{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Options.Applicative
import           System.FilePath.Lens


main :: IO ()
main = do
  cli@CLI {..} <- execParser cliParser
  orig <- either (error "Couldn't read image") convertRGBA8 <$> readImage cliPath
  when (invalidOpts cli orig)
    (error $ "Image dimension is " <> show (imageHeight orig) <> " rows by " <> show (imageWidth orig) <> " columns. "
          <> "Row/column min/max must be constrained by these bounds, and min must be less than, or equal to, max.")
  let outPath = makeFileName cliPath "-smeared"
  writePng outPath (makeSmearedImage cli orig)
  putStrLn outPath
  where
    invalidOpts CLI {..} orig =
      let rMin = fromMaybe 0 cliRowMin
          rMax = fromMaybe (imageHeight orig) cliRowMax
          cMin = fromMaybe 0 cliColMin
          cMax = fromMaybe (imageWidth orig) cliColMax
      in rMin < 0 || rMax > imageHeight orig || cMin < 0 || cMax > imageWidth orig || (cMin > cMax || rMin > rMax)

    cliParser = info (helper <*> parseCli) (header "pixel-smear")

    parseCli = CLI
      <$> strOption (long "file" <> help "Image to manipulate")
      <*> optional (option auto $ long "row-min" <> help "Row to start pixel smearing")
      <*> optional (option auto $ long "row-max" <> help "Row to end pixel smearing")
      <*> optional (option auto $ long "col-min" <> help "Column to start pixel smearing")
      <*> optional (option auto $ long "col-max" <> help "Column to end pixel smearing")

    makeFileName imgPath suffix =
      let baseDir     = imgPath ^. directory
          [name, ext] = case splitOn "." $ imgPath ^. filename of
            (n:x:_) -> [n, x]
            _       -> error "Invalid filename/extension."
      in baseDir <> "/" <> name <> suffix <> "." <> ext


data CLI = CLI
  { cliPath   :: FilePath
  , cliRowMin :: Maybe Int
  , cliRowMax :: Maybe Int
  , cliColMin :: Maybe Int
  , cliColMax :: Maybe Int
  } deriving (Eq, Show)


makeSmearedImage
  :: CLI
  -> Image PixelRGBA8
  -> Image PixelRGBA8
makeSmearedImage CLI {..} img@Image {..} = runST $ do
  mimg <- unsafeThawImage img
  let rMin = fromMaybe 0 cliRowMin
      rMax = fromMaybe imageHeight cliRowMax
      cMin = fromMaybe 0 cliColMin
      cMax = fromMaybe imageWidth cliColMax
      baseRow = makeBaseRow rMax cMin cMax img
  go rMin rMax cMin cMax imageWidth imageData baseRow mimg
  where
    go r r' c c' iw d baseRow mimg
      | r >= r' = unsafeFreezeImage mimg
      | otherwise = do
          void $ writeRow c c' c r baseRow mimg
          go (r + 1) r' c c' iw (VS.drop (4 * iw) d) baseRow mimg

    writeRow c c' ic r v mimg
      | c >= c' = unsafeFreezeImage mimg
      | otherwise = do
          writePixel mimg c r (v V.! (c - ic))
          writeRow (c + 1) c' ic r v mimg


makeBaseRow
  :: Int -- ^ Max row.
  -> Int -- ^ Min column.
  -> Int -- ^ Max column.
  -> Image PixelRGBA8
  -> V.Vector PixelRGBA8
makeBaseRow rMax cMin cMax Image {..} =
  let raw = VS.take (4 * (cMax - cMin)) $ VS.drop (4 * (rMax * imageWidth +  cMin)) imageData
  in go Seq.empty (4 * (cMax - cMin)) raw
  where
    go !acc !w !d
      | w == 0 = V.fromList $ toList acc
      | otherwise =
          go (acc Seq.|> makePixel (VS.take 4 d)) (w - 4) (VS.drop 4 d)

    makePixel d = PixelRGBA8 (d VS.! 0) (d VS.! 1) (d VS.! 2) (d VS.! 3)
