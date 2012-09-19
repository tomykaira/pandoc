{-
Copyright (C) 2012 tomykaira <tomykaira@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.PukiWiki
   Copyright   : Copyright (C) 2012 tomykaira
   License     : GNU GPL, version 2 or above

   Maintainer  : tomykaira <tomykaira@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to PukiWiki markup.

PukiWiki:  <http://pukiwiki.sourceforge.jp/>
-}
module Text.Pandoc.Writers.PukiWiki ( writePukiWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intercalate )
import Control.Monad.State

data WriterState = WriterState {
     stListLevel :: Int             -- Count the depth to insert \n on the end of a list
  }

-- | Convert Pandoc to PukiWiki.
writePukiWiki :: WriterOptions -> Pandoc -> String
writePukiWiki opts document =
  evalState (pandocToPukiWiki opts document)
            (WriterState { stListLevel = 0 })

-- | Return PukiWiki representation of document.
pandocToPukiWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToPukiWiki opts (Pandoc _ blocks) = do
  main <- blockListToPukiWiki opts blocks
  let context = writerVariables opts ++
                [ ("body", main) ] ++
                [ ("toc", "yes") | writerTableOfContents opts ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Escape special characters for PukiWiki.
escapeString :: String -> String
escapeString =  escapeStringForXML

-- | Convert Pandoc block element to PukiWiki.
blockToPukiWiki :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToPukiWiki _ Null = return ""

blockToPukiWiki opts (Plain inlines) =
  inlineListToPukiWiki opts inlines

blockToPukiWiki opts (Para [Image txt (src,tit)]) = do
  capt <- inlineListToPukiWiki opts txt
  let title = if null tit then capt else tit ++ capt
  return $ "#ref(" ++ src ++ "," ++ title ++ ")\n"

blockToPukiWiki opts (Para inlines) = do
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToPukiWiki opts inlines
  return $ contents ++ if listLevel == 0 then "\n" else ""

blockToPukiWiki _ (RawBlock "pukiWiki" str) = return str
blockToPukiWiki _ (RawBlock "html" str) = return str
blockToPukiWiki _ (RawBlock _ _) = return ""

blockToPukiWiki _ HorizontalRule = return "\n-----\n"

blockToPukiWiki opts (Header level inlines) = do
  contents <- inlineListToPukiWiki opts inlines
  let eqs = replicate level '*'
  return $ eqs ++ " " ++ contents ++ "\n"

blockToPukiWiki _ (CodeBlock (_,_,_) str) = do
  return $ prefix " " str ++ "\n"

blockToPukiWiki opts (BlockQuote blocks) = do
  contents <- blockListToPukiWiki opts blocks
  return $ prefix ">" contents ++ "\n"

-- No syntax for caption, width
blockToPukiWiki opts (Table caption aligns _ headers rows') = do
  captionStr <- if null caption
                then return ""
                else do
                  c <- inlineListToPukiWiki opts caption
                  return $ c ++ "\n"
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToPukiWiki opts aligns headers
                 return $ hs ++ "h"
  body' <- mapM (tableRowToPukiWiki opts aligns) rows'
  return $ captionStr ++ intercalate "\n" (head' : body')

blockToPukiWiki opts (BulletList items) = do
  listLevel <- get >>= return . stListLevel
  modify $ \s -> s { stListLevel = stListLevel s + 1 }
  contents <- mapM (listItemToPukiWiki opts "-") items
  modify $ \s -> s { stListLevel = stListLevel s - 1 }
  return $ vcat contents ++ if listLevel == 0 then "\n" else ""

blockToPukiWiki opts (OrderedList _ items) = do
  listLevel <- get >>= return . stListLevel
  modify $ \s -> s { stListLevel = stListLevel s + 1 }
  contents <- mapM (listItemToPukiWiki opts "+") items
  modify $ \s -> s { stListLevel = stListLevel s - 1 }
  return $ vcat contents ++ if listLevel == 0 then "\n" else ""

blockToPukiWiki opts (DefinitionList items) = do
  listLevel <- get >>= return . stListLevel
  modify $ \s -> s { stListLevel = stListLevel s + 1 }
  contents <- mapM (definitionListItemToPukiWiki opts) items
  modify $ \s -> s { stListLevel = stListLevel s - 1 }
  return $ vcat contents ++ if listLevel == 0 then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert bullet or ordered list item (list of blocks) to PukiWiki.
listItemToPukiWiki :: WriterOptions -> String -> [Block] -> State WriterState String
listItemToPukiWiki opts marker items = do
  contents <- blockListToPukiWiki opts items
  return $ marker ++ contents

-- | Convert definition list item (label, list of blocks) to PukiWiki.
definitionListItemToPukiWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToPukiWiki opts (label, items) = do
  labelText <- inlineListToPukiWiki opts label
  contents <- mapM (blockListToPukiWiki opts) items
  return $ ":" ++ labelText ++ "|" ++
      (intercalate "\n:|" contents)

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- | Prefix the specified string for every line.
prefix :: String -> String -> String
prefix pref str = pref ++ foldr (\c prefixed -> if isNewline c
                                                then c : pref ++ prefixed
                                                else c : prefixed) "" (stripTrailingNewlines str)
    where
      isNewline '\n' = True
      isNewline _    = False

-- Auxiliary functions for tables:

tableRowToPukiWiki :: WriterOptions
                    -> [Alignment]
                    -> [[Block]]
                    -> State WriterState String
tableRowToPukiWiki opts aligns cols' = do
  cols'' <- sequence $ zipWith
            (tableItemToPukiWiki opts)
            aligns cols'
  return $ "|" ++ intercalate "|" cols'' ++ "|"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> ""
                                 AlignRight   -> "RIGHT:"
                                 AlignCenter  -> "CENTER:"
                                 AlignDefault -> ""

tableItemToPukiWiki :: WriterOptions
                     -> Alignment
                     -> [Block]
                     -> State WriterState String
tableItemToPukiWiki opts align' item = do
  contents <- blockListToPukiWiki opts item
  return $ alignmentToString align' ++ contents

-- | Convert list of Pandoc block elements to PukiWiki.
blockListToPukiWiki :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToPukiWiki opts blocks =
  mapM (blockToPukiWiki opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to PukiWiki.
inlineListToPukiWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToPukiWiki opts lst =
  mapM (inlineToPukiWiki opts) lst >>= return . concat

-- | Convert Pandoc inline element to PukiWiki.
inlineToPukiWiki :: WriterOptions -> Inline -> State WriterState String

inlineToPukiWiki opts (Emph lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "'''" ++ contents ++ "'''"

inlineToPukiWiki opts (Strong lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "''" ++ contents ++ "''"

inlineToPukiWiki opts (Strikeout lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "%%" ++ contents ++ "%%"

-- not supported
inlineToPukiWiki opts (Superscript lst) = inlineListToPukiWiki opts lst

-- not supported
inlineToPukiWiki opts (Subscript lst) = inlineListToPukiWiki opts lst

inlineToPukiWiki opts (SmallCaps lst) = inlineListToPukiWiki opts lst

inlineToPukiWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToPukiWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToPukiWiki opts (Cite _  lst) = inlineListToPukiWiki opts lst

inlineToPukiWiki _ (Code _ str) =
  return $ "&color(midnightblue){" ++ str ++ "};"

inlineToPukiWiki _ (Str str) = return $ escapeString str

-- not supported by default
inlineToPukiWiki _ (Math _ str) = return $ prefix " " str
                                 -- note:  str should NOT be escaped

inlineToPukiWiki _ (RawInline "pukiWiki" str) = return str
inlineToPukiWiki _ (RawInline "html" str) = return str
inlineToPukiWiki _ (RawInline _ _) = return ""

inlineToPukiWiki _ (LineBreak) = return "&br;\n"

inlineToPukiWiki _ Space = return " "

inlineToPukiWiki opts (Link txt (src, _)) = do
  label <- inlineListToPukiWiki opts txt
  return $ "[[" ++ label ++ ">" ++ src ++ "]]"

inlineToPukiWiki opts (Image alt (src, tit)) = do
  capt <- inlineListToPukiWiki opts alt
  let title = if null tit then capt else tit ++ capt
  return $ "&ref(" ++ src ++ "," ++ title ++ ");"

inlineToPukiWiki opts (Note contents) = do
  contents' <- blockListToPukiWiki opts contents
  return $ "((" ++ contents' ++ "))"
  -- note - may not work for notes with multiple blocks
