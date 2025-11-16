{-# LANGUAGE OverloadedStrings #-}

module Rich.Tree
  ( -- * Tree Types
    Tree (..),
    TreeStyle (..),

    -- * Creating Trees
    tree,
    leaf,

    -- * Tree Operations
    addChild,
    setLabel,
    setStyle,

    -- * Rendering
    renderTree,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Rich.Style

-- | Tree style configuration
data TreeStyle = TreeStyle
  { treeGuideStyle :: Maybe Style,
    treeLabelStyle :: Maybe Style
  }
  deriving (Show, Eq)

-- | Default tree style
defaultTreeStyle :: TreeStyle
defaultTreeStyle =
  TreeStyle
    { treeGuideStyle = Just (dim style),
      treeLabelStyle = Nothing
    }

-- | A tree node with label and children
data Tree = Tree
  { treeLabel :: Text,
    treeChildren :: [Tree],
    treeTreeStyle :: TreeStyle
  }
  deriving (Show, Eq)

-- | Create a tree node with label
tree :: Text -> Tree
tree label =
  Tree
    { treeLabel = label,
      treeChildren = [],
      treeTreeStyle = defaultTreeStyle
    }

-- | Create a leaf node (tree with no children)
leaf :: Text -> Tree
leaf = tree

-- | Add a child to a tree node
addChild :: Tree -> Tree -> Tree
addChild child parent =
  parent {treeChildren = treeChildren parent ++ [child]}

-- | Set the label of a tree node
setLabel :: Text -> Tree -> Tree
setLabel label t = t {treeLabel = label}

-- | Set the style of a tree node
setStyle :: TreeStyle -> Tree -> Tree
setStyle s t = t {treeTreeStyle = s}

-- | Render a tree with guides
renderTree :: Tree -> Text
renderTree t = renderTreeInternal "" True t

-- | Internal function to render tree with prefix
renderTreeInternal :: Text -> Bool -> Tree -> Text
renderTreeInternal prefix isLast t =
  let guideStyle = treeGuideStyle (treeTreeStyle t)
      labelStyle = treeLabelStyle (treeTreeStyle t)

      -- Characters for tree drawing
      branch = if isLast then "└── " else "├── "
      extension = if isLast then "    " else "│   "

      -- Style the guide characters
      styledBranch = case guideStyle of
        Nothing -> branch
        Just s -> renderText s branch

      -- Style the label
      styledLabel = case labelStyle of
        Nothing -> treeLabel t
        Just s -> renderText s (treeLabel t)

      -- Render current node
      currentLine = prefix <> styledBranch <> styledLabel

      -- Render children
      children = treeChildren t
      childPrefix =
        prefix <> case guideStyle of
          Nothing -> extension
          Just s -> renderText s extension

      childLines =
        zipWith
          (\child idx -> renderTreeInternal childPrefix (idx == length children - 1) child)
          children
          [0 ..]

      allLines = currentLine : childLines
   in T.intercalate "\n" allLines
