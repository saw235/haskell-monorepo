{-# LANGUAGE OverloadedStrings #-}

module Rich.Table
  ( -- * Table Types
    Table (..),
    Column (..),
    Row,
    Cell (..),
    Align (..),
    BorderStyle (..),

    -- * Creating Tables
    table,
    simpleTable,

    -- * Adding Content
    addColumn,
    addRow,
    setTitle,
    setBorderStyle,

    -- * Rendering
    renderTable,
  )
where

import Data.List (intercalate, transpose)
import Data.Text (Text)
import qualified Data.Text as T
import Rich.Style

-- | Text alignment
data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Show, Eq)

-- | Border style for tables
data BorderStyle
  = NoBorder
  | SimpleBorder
  | RoundedBorder
  | DoubleBorder
  | HeavyBorder
  deriving (Show, Eq)

-- | A table cell with optional styling
data Cell = Cell
  { cellText :: Text,
    cellStyle :: Maybe Style
  }
  deriving (Show, Eq)

-- | A table column definition
data Column = Column
  { columnHeader :: Text,
    columnAlign :: Align,
    columnStyle :: Maybe Style,
    columnWidth :: Maybe Int
  }
  deriving (Show, Eq)

-- | A table row
type Row = [Cell]

-- | A complete table
data Table = Table
  { tableColumns :: [Column],
    tableRows :: [Row],
    tableTitle :: Maybe Text,
    tableBorderStyle :: BorderStyle,
    tableTitleStyle :: Maybe Style
  }
  deriving (Show, Eq)

-- | Create a new empty table
table :: Table
table =
  Table
    { tableColumns = [],
      tableRows = [],
      tableTitle = Nothing,
      tableBorderStyle = SimpleBorder,
      tableTitleStyle = Nothing
    }

-- | Create a simple table from headers and data
simpleTable :: [Text] -> [[Text]] -> Table
simpleTable headers rows =
  let cols = map (\h -> Column h AlignLeft Nothing Nothing) headers
      dataRows = map (map (\t -> Cell t Nothing)) rows
   in Table
        { tableColumns = cols,
          tableRows = dataRows,
          tableTitle = Nothing,
          tableBorderStyle = SimpleBorder,
          tableTitleStyle = Nothing
        }

-- | Add a column to the table
addColumn :: Column -> Table -> Table
addColumn col t = t {tableColumns = tableColumns t ++ [col]}

-- | Add a row to the table
addRow :: Row -> Table -> Table
addRow row t = t {tableRows = tableRows t ++ [row]}

-- | Set the table title
setTitle :: Text -> Table -> Table
setTitle title t = t {tableTitle = Just title}

-- | Set the border style
setBorderStyle :: BorderStyle -> Table -> Table
setBorderStyle bs t = t {tableBorderStyle = bs}

-- | Border characters for different styles
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

-- | Get border characters for a style
getBorderChars :: BorderStyle -> Maybe BorderChars
getBorderChars NoBorder = Nothing
getBorderChars SimpleBorder =
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
getBorderChars RoundedBorder =
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
getBorderChars DoubleBorder =
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
getBorderChars HeavyBorder =
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

-- | Calculate column widths
calculateWidths :: Table -> [Int]
calculateWidths t =
  let headers = map (T.length . columnHeader) (tableColumns t)
      dataWidths =
        if null (tableRows t)
          then replicate (length headers) 0
          else map maximum $ transpose $ map (map (T.length . cellText)) (tableRows t)
      combined = zipWith max headers dataWidths
   in map (\(w, col) -> maybe w id (columnWidth col)) (zip combined (tableColumns t))

-- | Render a horizontal line
renderHorizontalLine :: Maybe BorderChars -> [Int] -> Text -> Text -> Text -> Text
renderHorizontalLine Nothing _ _ _ _ = ""
renderHorizontalLine (Just bc) widths left middle right =
  let segments = map (\w -> T.replicate (w + 2) (bcHorizontal bc)) widths
   in left <> T.intercalate middle segments <> right

-- | Render a row of cells
renderRow :: Maybe BorderChars -> [Int] -> [Column] -> Row -> Text
renderRow maybeBc widths cols row =
  let vertical = maybe "" bcVertical maybeBc
      paddedCells =
        zipWith3
          ( \col width cell ->
              let aligned = alignText (columnAlign col) width (cellText cell)
                  styled = case cellStyle cell of
                    Nothing -> aligned
                    Just s -> renderText s aligned
               in " " <> styled <> " "
          )
          cols
          widths
          row
   in vertical <> T.intercalate vertical paddedCells <> vertical

-- | Render the table to text
renderTable :: Table -> Text
renderTable t =
  let widths = calculateWidths t
      maybeBc = getBorderChars (tableBorderStyle t)
      numCols = length (tableColumns t)

      -- Title
      titleLines = case tableTitle t of
        Nothing -> []
        Just title ->
          let totalWidth = sum widths + (numCols - 1) * 3 + 4
              titleStyled = case tableTitleStyle t of
                Nothing -> title
                Just s -> renderText s title
              centeredTitle = alignText AlignCenter (totalWidth - 4) titleStyled
           in [ maybe "" bcTopLeft maybeBc
                  <> " "
                  <> centeredTitle
                  <> " "
                  <> maybe "" bcTopRight maybeBc
              ]

      -- Top border
      topBorder = case maybeBc of
        Nothing -> []
        Just bc ->
          if null titleLines
            then [renderHorizontalLine maybeBc widths (bcTopLeft bc) (bcTeeDown bc) (bcTopRight bc)]
            else []

      -- Header
      headerRow =
        let headerCells = map (\col -> Cell (columnHeader col) (columnStyle col)) (tableColumns t)
         in [renderRow maybeBc widths (tableColumns t) headerCells]

      -- Header separator
      headerSep = case maybeBc of
        Nothing -> []
        Just bc -> [renderHorizontalLine maybeBc widths (bcTeeRight bc) (bcCross bc) (bcTeeLeft bc)]

      -- Data rows
      dataRows = map (renderRow maybeBc widths (tableColumns t)) (tableRows t)

      -- Bottom border
      bottomBorder = case maybeBc of
        Nothing -> []
        Just bc -> [renderHorizontalLine maybeBc widths (bcBottomLeft bc) (bcTeeUp bc) (bcBottomRight bc)]

      allLines = titleLines ++ topBorder ++ headerRow ++ headerSep ++ dataRows ++ bottomBorder
   in T.intercalate "\n" allLines
