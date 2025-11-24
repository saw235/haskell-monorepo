{-# LANGUAGE OverloadedStrings #-}

module RpgRuleset.Parser.Markdown
  ( parseMarkdownContent
  , extractContentAfterFrontmatter
  , MarkdownDocument(..)
  , HeadingInfo(..)
  ) where

import CMark
import Data.Text (Text)
import qualified Data.Text as T

-- | A parsed markdown document
data MarkdownDocument = MarkdownDocument
  { mdContent :: !Text          -- ^ Raw markdown content (after frontmatter)
  , mdHtml :: !Text             -- ^ Rendered HTML
  , mdPlainText :: !Text        -- ^ Plain text for indexing/search
  , mdHeadings :: ![HeadingInfo] -- ^ Extracted headings
  } deriving (Show, Eq)

-- | Information about a heading in the document
data HeadingInfo = HeadingInfo
  { hiLevel :: !Int    -- ^ Heading level (1-6)
  , hiText :: !Text    -- ^ Heading text
  } deriving (Show, Eq)

-- | Parse markdown content and extract useful information
parseMarkdownContent :: Text -> MarkdownDocument
parseMarkdownContent content =
  let node = commonmarkToNode [] content
      html = nodeToHtml [] node
      plainText = extractPlainText node
      headings = extractHeadings node
  in MarkdownDocument
    { mdContent = content
    , mdHtml = html
    , mdPlainText = plainText
    , mdHeadings = headings
    }

-- | Extract content after YAML frontmatter (between --- markers)
extractContentAfterFrontmatter :: Text -> Text
extractContentAfterFrontmatter input =
  let ls = T.lines input
  in case ls of
    ("---" : rest) ->
      case break (== "---") rest of
        (_, "---" : remaining) -> T.unlines remaining
        _ -> input
    _ -> input

-- | Extract plain text from a cmark node (for search indexing)
extractPlainText :: Node -> Text
extractPlainText = nodeToText
  where
    nodeToText (Node _ nodeType children) =
      case nodeType of
        TEXT t -> t
        CODE t -> t
        CODE_BLOCK _ t -> t
        SOFTBREAK -> " "
        LINEBREAK -> " "
        _ -> T.concat (map nodeToText children)

-- | Extract all headings from a cmark node
extractHeadings :: Node -> [HeadingInfo]
extractHeadings = collectHeadings
  where
    collectHeadings (Node _ nodeType children) =
      case nodeType of
        HEADING level ->
          let text = T.concat (map extractHeadingText children)
          in [HeadingInfo level text] ++ concatMap collectHeadings children
        _ -> concatMap collectHeadings children

    extractHeadingText (Node _ nodeType children) =
      case nodeType of
        TEXT t -> t
        CODE t -> t
        _ -> T.concat (map extractHeadingText children)

-- | Get the first heading as the document title (if no explicit title)
getDocumentTitle :: MarkdownDocument -> Maybe Text
getDocumentTitle doc =
  case filter ((== 1) . hiLevel) (mdHeadings doc) of
    (h:_) -> Just (hiText h)
    [] -> Nothing

-- | Check if content contains a specific text (case-insensitive)
contentContains :: Text -> MarkdownDocument -> Bool
contentContains needle doc =
  T.toLower needle `T.isInfixOf` T.toLower (mdPlainText doc)

-- | Get all text from a specific heading section
getSectionContent :: Text -> MarkdownDocument -> Maybe Text
getSectionContent _ _ = Nothing  -- Placeholder for future implementation