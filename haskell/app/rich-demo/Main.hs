{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rich
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  console <- newConsole

  -- Header
  printStyledLn console (bold . brightCyan $ style) "╔═══════════════════════════════════════════════════╗"
  printStyledLn console (bold . brightCyan $ style) "║   Rich Library Demo - Beautiful Terminal Output   ║"
  printStyledLn console (bold . brightCyan $ style) "╚═══════════════════════════════════════════════════╝"
  emptyLine console

  -- Demo 1: Styled Text
  rule console (Just "1. Styled Text")
  emptyLine console

  printStyledLn console (bold . red $ style) "Bold Red Text"
  printStyledLn console (italic . green $ style) "Italic Green Text"
  printStyledLn console (underline . blue $ style) "Underlined Blue Text"
  printStyledLn console (bold . italic . yellow $ style) "Bold Italic Yellow Text"
  printStyledLn console (strikethrough . magenta $ style) "Strikethrough Magenta Text"
  printStyledLn console (dim . cyan $ style) "Dim Cyan Text"
  emptyLine console

  -- Demo 2: Colors
  rule console (Just "2. Color Palette")
  emptyLine console

  let colors =
        [ ("Black", black),
          ("Red", red),
          ("Green", green),
          ("Yellow", yellow),
          ("Blue", blue),
          ("Magenta", magenta),
          ("Cyan", cyan),
          ("White", white)
        ]
  forM_ colors $ \(name, colorFn) -> do
    printStyled console (colorFn style) ("█ " <> name)
    printLn console "  "

  emptyLine console

  let brightColors =
        [ ("Bright Black", brightBlack),
          ("Bright Red", brightRed),
          ("Bright Green", brightGreen),
          ("Bright Yellow", brightYellow),
          ("Bright Blue", brightBlue),
          ("Bright Magenta", brightMagenta),
          ("Bright Cyan", brightCyan),
          ("Bright White", brightWhite)
        ]
  forM_ brightColors $ \(name, colorFn) -> do
    printStyled console (colorFn style) ("█ " <> name)
    printLn console "  "

  emptyLine console

  -- Demo 3: Console Messages
  rule console (Just "3. Console Messages")
  emptyLine console

  printSuccess console "Operation completed successfully!"
  printError console "Something went wrong!"
  printWarning console "This is a warning message"
  printInfo console "Here's some useful information"
  emptyLine console

  -- Demo 4: Tables
  rule console (Just "4. Tables")
  emptyLine console

  let peopleTable =
        setBorderStyle RoundedBorder $
          setTitle "Team Members" $
            simpleTable
              ["Name", "Role", "Experience"]
              [ ["Alice Johnson", "Senior Developer", "8 years"],
                ["Bob Smith", "Designer", "5 years"],
                ["Charlie Brown", "Product Manager", "10 years"],
                ["Diana Prince", "Data Scientist", "6 years"]
              ]

  TIO.putStrLn (renderTable peopleTable)
  emptyLine console

  -- Demo 5: Different Border Styles
  rule console (Just "5. Border Styles")
  emptyLine console

  let simpleBorderTable = setBorderStyle SimpleBorder $ simpleTable ["Style"] [["Simple"]]
  TIO.putStrLn (renderTable simpleBorderTable)
  emptyLine console

  let roundedBorderTable = setBorderStyle RoundedBorder $ simpleTable ["Style"] [["Rounded"]]
  TIO.putStrLn (renderTable roundedBorderTable)
  emptyLine console

  let doubleBorderTable = setBorderStyle DoubleBorder $ simpleTable ["Style"] [["Double"]]
  TIO.putStrLn (renderTable doubleBorderTable)
  emptyLine console

  let heavyBorderTable = setBorderStyle HeavyBorder $ simpleTable ["Style"] [["Heavy"]]
  TIO.putStrLn (renderTable heavyBorderTable)
  emptyLine console

  -- Demo 6: Panels
  rule console (Just "6. Panels")
  emptyLine console

  let welcomePanel =
        simplePanel
          "Welcome to Rich!"
          "This is a beautiful panel with a title.\nPanels are great for highlighting important content.\nThey support multi-line text!"

  TIO.putStrLn (renderPanel welcomePanel)
  emptyLine console

  let infoPanel =
        setBorderStyle DoubleBorder $
          simplePanel
            "Important Notice"
            "Panels can have different border styles too!"

  TIO.putStrLn (renderPanel infoPanel)
  emptyLine console

  -- Demo 7: Trees
  rule console (Just "7. Tree Structures")
  emptyLine console

  let fileTree =
        addChild (leaf "README.md") $
          addChild (leaf "LICENSE") $
            addChild
              ( addChild (leaf "Main.hs") $
                  addChild (leaf "Utils.hs") $
                    tree "src"
              )
              $ addChild
                ( addChild (leaf "main_test.hs") $
                    tree "test"
                )
              $ tree "project"

  TIO.putStrLn (renderTree fileTree)
  emptyLine console

  -- Demo 8: Progress Bars
  rule console (Just "8. Progress Bars")
  emptyLine console

  printLn console "Simulating a long task..."
  emptyLine console

  let total = 50 :: Double
  forM_ [0 .. round total] $ \i -> do
    let progress =
          setCurrent (fromIntegral i) $
            setDescription "Processing" $
              setWidth 50 $
                progressBar total
    putStr "\r"
    TIO.putStr (renderProgressWithPercentage progress)
    hFlush stdout
    threadDelay 30000

  putStrLn ""
  printSuccess console "Task completed!"
  emptyLine console

  -- Demo 9: Multiple Progress Bars
  rule console (Just "9. Multiple Tasks")
  emptyLine console

  let tasks =
        [ ("Downloading files", 80),
          ("Installing packages", 60),
          ("Building project", 100)
        ]

  forM_ tasks $ \(desc, total) -> do
    forM_ [0 .. round total] $ \i -> do
      let progress =
            setCurrent (fromIntegral i) $
              setDescription desc $
                setWidth 40 $
                  progressBar total
      putStr "\r"
      TIO.putStr (renderProgressWithPercentage progress)
      hFlush stdout
      threadDelay 10000
    putStrLn ""
    printSuccess console (desc <> " - Done!")

  emptyLine console

  -- Demo 10: Combined Features
  rule console (Just "10. Combined Features")
  emptyLine console

  let summaryPanel =
        setBorderStyle RoundedBorder $
          simplePanel
            "Demo Summary"
            "You've seen:\n\
            \  ✓ Styled text with colors and formatting\n\
            \  ✓ Beautiful tables with multiple border styles\n\
            \  ✓ Informative panels and boxes\n\
            \  ✓ Tree structures for hierarchical data\n\
            \  ✓ Animated progress bars\n\
            \\n\
            \The Rich library makes terminal output beautiful!"

  TIO.putStrLn (renderPanel summaryPanel)
  emptyLine console

  -- Footer
  rule console Nothing
  printStyledLn console (bold . brightGreen $ style) "Thank you for trying Rich! 🎨"
  rule console Nothing
