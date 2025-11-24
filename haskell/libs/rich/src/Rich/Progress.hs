{-# LANGUAGE OverloadedStrings #-}

module Rich.Progress
  ( -- * Progress Bar Types
    ProgressBar (..),
    ProgressStyle (..),

    -- * Creating Progress Bars
    progressBar,
    simpleProgress,

    -- * Progress Bar Modifiers
    setTotal,
    setCurrent,
    setDescription,
    setStyle,
    setWidth,

    -- * Rendering
    renderProgress,
    renderProgressWithPercentage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Rich.Style
import Text.Printf (printf)

-- | Progress bar style configuration
data ProgressStyle = ProgressStyle
  { progressCompleteStyle :: Maybe Style,
    progressIncompleteStyle :: Maybe Style,
    progressDescriptionStyle :: Maybe Style,
    progressPercentageStyle :: Maybe Style,
    progressCompleteChar :: Text,
    progressIncompleteChar :: Text
  }
  deriving (Show, Eq)

-- | Default progress style with green complete, dim incomplete
defaultProgressStyle :: ProgressStyle
defaultProgressStyle =
  ProgressStyle
    { progressCompleteStyle = Just (green style),
      progressIncompleteStyle = Just (dim style),
      progressDescriptionStyle = Just (bold style),
      progressPercentageStyle = Nothing,
      progressCompleteChar = "█",
      progressIncompleteChar = "░"
    }

-- | A progress bar
data ProgressBar = ProgressBar
  { progressCurrent :: Double,
    progressTotal :: Double,
    progressDescription :: Maybe Text,
    progressBarWidth :: Int,
    progressProgressStyle :: ProgressStyle
  }
  deriving (Show, Eq)

-- | Create a progress bar with total
progressBar :: Double -> ProgressBar
progressBar total =
  ProgressBar
    { progressCurrent = 0,
      progressTotal = total,
      progressDescription = Nothing,
      progressBarWidth = 40,
      progressProgressStyle = defaultProgressStyle
    }

-- | Create a simple progress bar from current and total
simpleProgress :: Double -> Double -> ProgressBar
simpleProgress current total =
  ProgressBar
    { progressCurrent = current,
      progressTotal = total,
      progressDescription = Nothing,
      progressBarWidth = 40,
      progressProgressStyle = defaultProgressStyle
    }

-- | Set the total value
setTotal :: Double -> ProgressBar -> ProgressBar
setTotal total p = p {progressTotal = total}

-- | Set the current value
setCurrent :: Double -> ProgressBar -> ProgressBar
setCurrent current p = p {progressCurrent = current}

-- | Set the description
setDescription :: Text -> ProgressBar -> ProgressBar
setDescription desc p = p {progressDescription = Just desc}

-- | Set the progress style
setStyle :: ProgressStyle -> ProgressBar -> ProgressBar
setStyle s p = p {progressProgressStyle = s}

-- | Set the bar width
setWidth :: Int -> ProgressBar -> ProgressBar
setWidth width p = p {progressBarWidth = width}

-- | Calculate percentage (0-100)
percentage :: ProgressBar -> Double
percentage p =
  if progressTotal p <= 0
    then 0
    else min 100 $ (progressCurrent p / progressTotal p) * 100

-- | Render the progress bar
renderProgress :: ProgressBar -> Text
renderProgress p =
  let pct = percentage p
      pctInt = round pct :: Int
      width = progressBarWidth p
      completeWidth = (width * pctInt) `div` 100
      incompleteWidth = width - completeWidth

      pStyle = progressProgressStyle p
      completeChar = progressCompleteChar pStyle
      incompleteChar = progressIncompleteChar pStyle

      completePart = T.replicate completeWidth completeChar
      incompletePart = T.replicate incompleteWidth incompleteChar

      styledComplete = case progressCompleteStyle pStyle of
        Nothing -> completePart
        Just s -> renderText s completePart

      styledIncomplete = case progressIncompleteStyle pStyle of
        Nothing -> incompletePart
        Just s -> renderText s incompletePart

      bar = styledComplete <> styledIncomplete

      desc = case progressDescription p of
        Nothing -> ""
        Just d -> case progressDescriptionStyle pStyle of
          Nothing -> d <> " "
          Just s -> renderText s d <> " "
   in desc <> bar

-- | Render the progress bar with percentage display
renderProgressWithPercentage :: ProgressBar -> Text
renderProgressWithPercentage p =
  let bar = renderProgress p
      pct = percentage p
      pctText = T.pack $ printf "%3.0f%%" pct
      pStyle = progressProgressStyle p

      styledPct = case progressPercentageStyle pStyle of
        Nothing -> pctText
        Just s -> renderText s pctText
   in bar <> " " <> styledPct
