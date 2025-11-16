{-# LANGUAGE OverloadedStrings #-}

module Rich.Style
  ( -- * Style Types
    Style (..),
    Color (..),
    StyleAttr (..),

    -- * Creating Styles
    defaultStyle,
    style,

    -- * Style Modifiers
    bold,
    italic,
    underline,
    dim,
    blink,
    reverse,
    hidden,
    strikethrough,

    -- * Colors
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,
    rgb,

    -- * Background Colors
    onBlack,
    onRed,
    onGreen,
    onYellow,
    onBlue,
    onMagenta,
    onCyan,
    onWhite,
    onBrightBlack,
    onBrightRed,
    onBrightGreen,
    onBrightYellow,
    onBrightBlue,
    onBrightMagenta,
    onBrightCyan,
    onBrightWhite,
    onRgb,

    -- * Rendering
    render,
    renderText,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Represents a color in the terminal
data Color
  = -- | Standard ANSI colors
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | -- | Bright ANSI colors
    BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  | -- | RGB color
    RGB Word8 Word8 Word8
  deriving (Show, Eq)

-- | Style attributes
data StyleAttr
  = Bold
  | Italic
  | Underline
  | Dim
  | Blink
  | Reverse
  | Hidden
  | Strikethrough
  deriving (Show, Eq, Ord)

-- | Complete style specification
data Style = Style
  { styleForeground :: Maybe Color,
    styleBackground :: Maybe Color,
    styleAttrs :: [StyleAttr]
  }
  deriving (Show, Eq)

-- | Default style with no formatting
defaultStyle :: Style
defaultStyle =
  Style
    { styleForeground = Nothing,
      styleBackground = Nothing,
      styleAttrs = []
    }

-- | Create a style from scratch
style :: Style
style = defaultStyle

-- | Add bold attribute to style
bold :: Style -> Style
bold s = s {styleAttrs = Bold : styleAttrs s}

-- | Add italic attribute to style
italic :: Style -> Style
italic s = s {styleAttrs = Italic : styleAttrs s}

-- | Add underline attribute to style
underline :: Style -> Style
underline s = s {styleAttrs = Underline : styleAttrs s}

-- | Add dim attribute to style
dim :: Style -> Style
dim s = s {styleAttrs = Dim : styleAttrs s}

-- | Add blink attribute to style
blink :: Style -> Style
blink s = s {styleAttrs = Blink : styleAttrs s}

-- | Add reverse attribute to style
reverse :: Style -> Style
reverse s = s {styleAttrs = Reverse : styleAttrs s}

-- | Add hidden attribute to style
hidden :: Style -> Style
hidden s = s {styleAttrs = Hidden : styleAttrs s}

-- | Add strikethrough attribute to style
strikethrough :: Style -> Style
strikethrough s = s {styleAttrs = Strikethrough : styleAttrs s}

-- | Set foreground color to black
black :: Style -> Style
black s = s {styleForeground = Just Black}

-- | Set foreground color to red
red :: Style -> Style
red s = s {styleForeground = Just Red}

-- | Set foreground color to green
green :: Style -> Style
green s = s {styleForeground = Just Green}

-- | Set foreground color to yellow
yellow :: Style -> Style
yellow s = s {styleForeground = Just Yellow}

-- | Set foreground color to blue
blue :: Style -> Style
blue s = s {styleForeground = Just Blue}

-- | Set foreground color to magenta
magenta :: Style -> Style
magenta s = s {styleForeground = Just Magenta}

-- | Set foreground color to cyan
cyan :: Style -> Style
cyan s = s {styleForeground = Just Cyan}

-- | Set foreground color to white
white :: Style -> Style
white s = s {styleForeground = Just White}

-- | Set foreground color to bright black (gray)
brightBlack :: Style -> Style
brightBlack s = s {styleForeground = Just BrightBlack}

-- | Set foreground color to bright red
brightRed :: Style -> Style
brightRed s = s {styleForeground = Just BrightRed}

-- | Set foreground color to bright green
brightGreen :: Style -> Style
brightGreen s = s {styleForeground = Just BrightGreen}

-- | Set foreground color to bright yellow
brightYellow :: Style -> Style
brightYellow s = s {styleForeground = Just BrightYellow}

-- | Set foreground color to bright blue
brightBlue :: Style -> Style
brightBlue s = s {styleForeground = Just BrightBlue}

-- | Set foreground color to bright magenta
brightMagenta :: Style -> Style
brightMagenta s = s {styleForeground = Just BrightMagenta}

-- | Set foreground color to bright cyan
brightCyan :: Style -> Style
brightCyan s = s {styleForeground = Just BrightCyan}

-- | Set foreground color to bright white
brightWhite :: Style -> Style
brightWhite s = s {styleForeground = Just BrightWhite}

-- | Set foreground color to RGB
rgb :: Word8 -> Word8 -> Word8 -> Style -> Style
rgb r g b s = s {styleForeground = Just (RGB r g b)}

-- | Set background color to black
onBlack :: Style -> Style
onBlack s = s {styleBackground = Just Black}

-- | Set background color to red
onRed :: Style -> Style
onRed s = s {styleBackground = Just Red}

-- | Set background color to green
onGreen :: Style -> Style
onGreen s = s {styleBackground = Just Green}

-- | Set background color to yellow
onYellow :: Style -> Style
onYellow s = s {styleBackground = Just Yellow}

-- | Set background color to blue
onBlue :: Style -> Style
onBlue s = s {styleBackground = Just Blue}

-- | Set background color to magenta
onMagenta :: Style -> Style
onMagenta s = s {styleBackground = Just Magenta}

-- | Set background color to cyan
onCyan :: Style -> Style
onCyan s = s {styleBackground = Just Cyan}

-- | Set background color to white
onWhite :: Style -> Style
onWhite s = s {styleBackground = Just White}

-- | Set background color to bright black
onBrightBlack :: Style -> Style
onBrightBlack s = s {styleBackground = Just BrightBlack}

-- | Set background color to bright red
onBrightRed :: Style -> Style
onBrightRed s = s {styleBackground = Just BrightRed}

-- | Set background color to bright green
onBrightGreen :: Style -> Style
onBrightGreen s = s {styleBackground = Just BrightGreen}

-- | Set background color to bright yellow
onBrightYellow :: Style -> Style
onBrightYellow s = s {styleBackground = Just BrightYellow}

-- | Set background color to bright blue
onBrightBlue :: Style -> Style
onBrightBlue s = s {styleBackground = Just BrightBlue}

-- | Set background color to bright magenta
onBrightMagenta :: Style -> Style
onBrightMagenta s = s {styleBackground = Just BrightMagenta}

-- | Set background color to bright cyan
onBrightCyan :: Style -> Style
onBrightCyan s = s {styleBackground = Just BrightCyan}

-- | Set background color to bright white
onBrightWhite :: Style -> Style
onBrightWhite s = s {styleBackground = Just BrightWhite}

-- | Set background color to RGB
onRgb :: Word8 -> Word8 -> Word8 -> Style -> Style
onRgb r g b s = s {styleBackground = Just (RGB r g b)}

-- | Convert a color to its ANSI foreground code
colorToForeground :: Color -> String
colorToForeground c = case c of
  Black -> "30"
  Red -> "31"
  Green -> "32"
  Yellow -> "33"
  Blue -> "34"
  Magenta -> "35"
  Cyan -> "36"
  White -> "37"
  BrightBlack -> "90"
  BrightRed -> "91"
  BrightGreen -> "92"
  BrightYellow -> "93"
  BrightBlue -> "94"
  BrightMagenta -> "95"
  BrightCyan -> "96"
  BrightWhite -> "97"
  RGB r g b -> "38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b

-- | Convert a color to its ANSI background code
colorToBackground :: Color -> String
colorToBackground c = case c of
  Black -> "40"
  Red -> "41"
  Green -> "42"
  Yellow -> "43"
  Blue -> "44"
  Magenta -> "45"
  Cyan -> "46"
  White -> "47"
  BrightBlack -> "100"
  BrightRed -> "101"
  BrightGreen -> "102"
  BrightYellow -> "103"
  BrightBlue -> "104"
  BrightMagenta -> "105"
  BrightCyan -> "106"
  BrightWhite -> "107"
  RGB r g b -> "48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b

-- | Convert a style attribute to its ANSI code
attrToCode :: StyleAttr -> String
attrToCode attr = case attr of
  Bold -> "1"
  Dim -> "2"
  Italic -> "3"
  Underline -> "4"
  Blink -> "5"
  Reverse -> "7"
  Hidden -> "8"
  Strikethrough -> "9"

-- | Generate ANSI escape sequence for a style
styleToAnsi :: Style -> String
styleToAnsi s =
  let codes = concat [attrCodes, fgCodes, bgCodes]
   in if null codes
        then ""
        else "\ESC[" ++ intercalate ";" codes ++ "m"
  where
    attrCodes = map attrToCode (styleAttrs s)
    fgCodes = maybe [] (\c -> [colorToForeground c]) (styleForeground s)
    bgCodes = maybe [] (\c -> [colorToBackground c]) (styleBackground s)

-- | Reset ANSI formatting
reset :: String
reset = "\ESC[0m"

-- | Render text with a given style
render :: Style -> String -> String
render s text =
  let ansi = styleToAnsi s
   in if null ansi
        then text
        else ansi ++ text ++ reset

-- | Render Text with a given style
renderText :: Style -> Text -> Text
renderText s text =
  let ansi = T.pack $ styleToAnsi s
      resetCode = T.pack reset
   in if T.null ansi
        then text
        else ansi <> text <> resetCode
