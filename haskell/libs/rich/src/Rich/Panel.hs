{-# LANGUAGE OverloadedStrings #-}

module Rich.Panel
  ( -- * Panel Types
    Panel (..),
    PanelStyle (..),
    Padding (..),

    -- * Creating Panels
    panel,
    simplePanel,

    -- * Panel Modifiers
    setTitle,
    setSubtitle,
    setPadding,
    setStyle,
    setBorderStyle,

    -- * Rendering
    renderPanel,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Rich.Style
import Rich.Table (Align (..), BorderStyle (..), getBorderChars)

-- | Padding specification
data Padding = Padding
  { paddingTop :: Int,
    paddingRight :: Int,
    paddingBottom :: Int,
    paddingLeft :: Int
  }
  deriving (Show, Eq)

-- | Default padding (1 on all sides)
defaultPadding :: Padding
defaultPadding = Padding 1 2 1 2

-- | Panel style configuration
data PanelStyle = PanelStyle
  { panelBorderStyle :: BorderStyle,
    panelBorderColor :: Maybe Style,
    panelTitleStyle :: Maybe Style,
    panelContentStyle :: Maybe Style
  }
  deriving (Show, Eq)

-- | Default panel style
defaultPanelStyle :: PanelStyle
defaultPanelStyle =
  PanelStyle
    { panelBorderStyle = RoundedBorder,
      panelBorderColor = Nothing,
      panelTitleStyle = Just (bold style),
      panelContentStyle = Nothing
    }

-- | A panel with title, content, and styling
data Panel = Panel
  { panelTitle :: Maybe Text,
    panelSubtitle :: Maybe Text,
    panelContent :: Text,
    panelPadding :: Padding,
    panelPanelStyle :: PanelStyle,
    panelWidth :: Maybe Int
  }
  deriving (Show, Eq)

-- | Create a new panel with content
panel :: Text -> Panel
panel content =
  Panel
    { panelTitle = Nothing,
      panelSubtitle = Nothing,
      panelContent = content,
      panelPadding = defaultPadding,
      panelPanelStyle = defaultPanelStyle,
      panelWidth = Nothing
    }

-- | Create a simple panel with title and content
simplePanel :: Text -> Text -> Panel
simplePanel title content =
  Panel
    { panelTitle = Just title,
      panelSubtitle = Nothing,
      panelContent = content,
      panelPadding = defaultPadding,
      panelPanelStyle = defaultPanelStyle,
      panelWidth = Nothing
    }

-- | Set the panel title
setTitle :: Text -> Panel -> Panel
setTitle title p = p {panelTitle = Just title}

-- | Set the panel subtitle
setSubtitle :: Text -> Panel -> Panel
setSubtitle subtitle p = p {panelSubtitle = Just subtitle}

-- | Set the panel padding
setPadding :: Padding -> Panel -> Panel
setPadding padding p = p {panelPadding = padding}

-- | Set the panel style
setStyle :: PanelStyle -> Panel -> Panel
setStyle s p = p {panelPanelStyle = s}

-- | Set the border style
setBorderStyle :: BorderStyle -> Panel -> Panel
setBorderStyle bs p =
  let currentStyle = panelPanelStyle p
   in p {panelPanelStyle = currentStyle {panelBorderStyle = bs}}

-- | Import border chars from Rich.Table
data BorderChars = BorderChars
  { bcTopLeft :: Text,
    bcTopRight :: Text,
    bcBottomLeft :: Text,
    bcBottomRight :: Text,
    bcHorizontal :: Text,
    bcVertical :: Text,
    bcCross :: Text,
    bcTeeDown :: Text,
    bcTeeUp :: Text,
    bcTeeLeft :: Text,
    bcTeeRight :: Text
  }

-- | Get border characters (duplicated from Rich.Table for now)
localGetBorderChars :: BorderStyle -> Maybe BorderChars
localGetBorderChars NoBorder = Nothing
localGetBorderChars SimpleBorder =
  Just $
    BorderChars
      { bcTopLeft = "┌",
        bcTopRight = "┐",
        bcBottomLeft = "└",
        bcBottomRight = "┘",
        bcHorizontal = "─",
        bcVertical = "│",
        bcCross = "┼",
        bcTeeDown = "┬",
        bcTeeUp = "┴",
        bcTeeLeft = "┤",
        bcTeeRight = "├"
      }
localGetBorderChars RoundedBorder =
  Just $
    BorderChars
      { bcTopLeft = "╭",
        bcTopRight = "╮",
        bcBottomLeft = "╰",
        bcBottomRight = "╯",
        bcHorizontal = "─",
        bcVertical = "│",
        bcCross = "┼",
        bcTeeDown = "┬",
        bcTeeUp = "┴",
        bcTeeLeft = "┤",
        bcTeeRight = "├"
      }
localGetBorderChars DoubleBorder =
  Just $
    BorderChars
      { bcTopLeft = "╔",
        bcTopRight = "╗",
        bcBottomLeft = "╚",
        bcBottomRight = "╝",
        bcHorizontal = "═",
        bcVertical = "║",
        bcCross = "╬",
        bcTeeDown = "╦",
        bcTeeUp = "╩",
        bcTeeLeft = "╣",
        bcTeeRight = "╠"
      }
localGetBorderChars HeavyBorder =
  Just $
    BorderChars
      { bcTopLeft = "┏",
        bcTopRight = "┓",
        bcBottomLeft = "┗",
        bcBottomRight = "┛",
        bcHorizontal = "━",
        bcVertical = "┃",
        bcCross = "╋",
        bcTeeDown = "┳",
        bcTeeUp = "┻",
        bcTeeLeft = "┫",
        bcTeeRight = "┣"
      }

-- | Align text within a width
alignText :: Align -> Int -> Text -> Text
alignText align width text =
  let len = T.length text
      padding = max 0 (width - len)
   in case align of
        AlignLeft -> text <> T.replicate padding " "
        AlignRight -> T.replicate padding " " <> text
        AlignCenter ->
          let leftPad = padding `div` 2
              rightPad = padding - leftPad
           in T.replicate leftPad " " <> text <> T.replicate rightPad " "

-- | Render border character with optional style
renderBorderChar :: Maybe Style -> Text -> Text
renderBorderChar Nothing char = char
renderBorderChar (Just s) char = renderText s char

-- | Render the panel to text
renderPanel :: Panel -> Text
renderPanel p =
  let maybeBc = localGetBorderChars (panelBorderStyle (panelPanelStyle p))
      borderColor = panelBorderColor (panelPanelStyle p)
      titleStyle = panelTitleStyle (panelPanelStyle p)
      contentStyle = panelContentStyle (panelPanelStyle p)

      -- Split content into lines
      contentLines = T.lines (panelContent p)

      -- Calculate content width
      maxContentWidth = if null contentLines then 0 else maximum (map T.length contentLines)
      padding = panelPadding p
      innerWidth = maxContentWidth + paddingLeft padding + paddingRight padding
      totalWidth = case panelWidth p of
        Just w -> max w innerWidth
        Nothing -> innerWidth

      contentWidth = totalWidth - paddingLeft padding - paddingRight padding

      -- Render title line
      titleLine = case (panelTitle p, maybeBc) of
        (Just title, Just bc) ->
          let styledTitle = case titleStyle of
                Nothing -> title
                Just s -> renderText s title
              titleLen = T.length title + 2 -- +2 for spaces
              leftLineLen = 2
              rightLineLen = totalWidth - leftLineLen - titleLen
              leftLine = renderBorderChar borderColor (bcTopLeft bc <> T.replicate leftLineLen (bcHorizontal bc))
              rightLine = renderBorderChar borderColor (T.replicate rightLineLen (bcHorizontal bc) <> bcTopRight bc)
           in [leftLine <> " " <> styledTitle <> " " <> rightLine]
        (Nothing, Just bc) ->
          let line = renderBorderChar borderColor (bcTopLeft bc <> T.replicate totalWidth (bcHorizontal bc) <> bcTopRight bc)
           in [line]
        _ -> []

      -- Top padding lines
      topPaddingLines = case maybeBc of
        Just bc ->
          replicate
            (paddingTop padding)
            ( renderBorderChar borderColor (bcVertical bc)
                <> T.replicate totalWidth " "
                <> renderBorderChar borderColor (bcVertical bc)
            )
        Nothing -> replicate (paddingTop padding) ""

      -- Content lines with side padding
      paddedContentLines = case maybeBc of
        Just bc ->
          map
            ( \line ->
                let padded = T.replicate (paddingLeft padding) " " <> alignText AlignLeft contentWidth line <> T.replicate (paddingRight padding) " "
                    styled = case contentStyle of
                      Nothing -> padded
                      Just s -> renderText s padded
                 in renderBorderChar borderColor (bcVertical bc) <> styled <> renderBorderChar borderColor (bcVertical bc)
            )
            contentLines
        Nothing ->
          map
            ( \line ->
                let padded = T.replicate (paddingLeft padding) " " <> alignText AlignLeft contentWidth line <> T.replicate (paddingRight padding) " "
                 in case contentStyle of
                      Nothing -> padded
                      Just s -> renderText s padded
            )
            contentLines

      -- Bottom padding lines
      bottomPaddingLines = case maybeBc of
        Just bc ->
          replicate
            (paddingBottom padding)
            ( renderBorderChar borderColor (bcVertical bc)
                <> T.replicate totalWidth " "
                <> renderBorderChar borderColor (bcVertical bc)
            )
        Nothing -> replicate (paddingBottom padding) ""

      -- Bottom border
      bottomLine = case maybeBc of
        Just bc ->
          [renderBorderChar borderColor (bcBottomLeft bc <> T.replicate totalWidth (bcHorizontal bc) <> bcBottomRight bc)]
        Nothing -> []

      allLines = titleLine ++ topPaddingLines ++ paddedContentLines ++ bottomPaddingLines ++ bottomLine
   in T.intercalate "\n" allLines
