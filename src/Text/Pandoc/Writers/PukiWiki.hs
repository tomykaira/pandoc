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
import Text.Pandoc.Generic
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intersect, intercalate )
import Network.URI ( isURI )
import Control.Monad.State

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stListLevel :: [Char]          -- String at beginning of list items, e.g. "**"
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to PukiWiki.
writePukiWiki :: WriterOptions -> Pandoc -> String
writePukiWiki opts document =
  evalState (pandocToPukiWiki opts document)
            (WriterState { stNotes = False, stListLevel = [], stUseTags = False })

-- | Return PukiWiki representation of document.
pandocToPukiWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToPukiWiki opts (Pandoc _ blocks) = do
  body <- blockListToPukiWiki opts blocks
  notesExist <- get >>= return . stNotes
  let notes = if notesExist
                 then "\n<references />"
                 else ""
  let main = body ++ notes
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
  capt <- if null txt
             then return ""
             else ("|caption " ++) `fmap` inlineListToPukiWiki opts txt
  let opt = if null txt
               then ""
               else "|alt=" ++ if null tit then capt else tit ++ capt
  return $ "[[Image:" ++ src ++ "|frame|none" ++ opt ++ "]]\n"

blockToPukiWiki opts (Para inlines) = do
  useTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToPukiWiki opts inlines
  return $ if useTags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null listLevel then "\n" else ""

blockToPukiWiki _ (RawBlock "pukiWiki" str) = return str
blockToPukiWiki _ (RawBlock "html" str) = return str
blockToPukiWiki _ (RawBlock _ _) = return ""

blockToPukiWiki _ HorizontalRule = return "\n-----\n"

blockToPukiWiki opts (Header level inlines) = do
  contents <- inlineListToPukiWiki opts inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToPukiWiki _ (CodeBlock (_,classes,_) str) = do
  return $ prefix " " str ++ "\n"

blockToPukiWiki opts (BlockQuote blocks) = do
  contents <- blockListToPukiWiki opts blocks
  return $ prefix ">" contents ++ "\n"

blockToPukiWiki opts (Table capt aligns widths headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToPukiWiki opts capt
                      return $ "<caption>" ++ c ++ "</caption>\n"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then ""
                   else unlines $ map
                         (\w -> "<col width=\"" ++ percent w ++ "\" />") widths
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToPukiWiki opts alignStrings 0 headers
                 return $ "<thead>\n" ++ hs ++ "\n</thead>\n"
  body' <- zipWithM (tableRowToPukiWiki opts alignStrings) [1..] rows'
  return $ "<table>\n" ++ captionDoc ++ coltags ++ head' ++
            "<tbody>\n" ++ unlines body' ++ "</tbody>\n</table>\n"

blockToPukiWiki opts x@(BulletList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToPukiWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "*" }
        contents <- mapM (listItemToPukiWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

blockToPukiWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToPukiWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "#" }
        contents <- mapM (listItemToPukiWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

blockToPukiWiki opts x@(DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (definitionListItemToPukiWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ ";" }
        contents <- mapM (definitionListItemToPukiWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToString :: ListAttributes -> String
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ show numstyle
  in  (if startnum /= 1
          then " start=\"" ++ show startnum ++ "\""
          else "") ++
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " ++ numstyle' ++ ";\""
          else "")

-- | Convert bullet or ordered list item (list of blocks) to PukiWiki.
listItemToPukiWiki :: WriterOptions -> [Block] -> State WriterState String
listItemToPukiWiki opts items = do
  contents <- blockListToPukiWiki opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to PukiWiki.
definitionListItemToPukiWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToPukiWiki opts (label, items) = do
  labelText <- inlineListToPukiWiki opts label
  contents <- mapM (blockListToPukiWiki opts) items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<dt>" ++ labelText ++ "</dt>\n" ++
           (intercalate "\n" $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ labelText ++ "\n" ++
           (intercalate "\n" $ map (\d -> init marker ++ ": " ++ d) contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
       DefinitionList items             -> all isSimpleListItem $ concatMap snd items
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _           -> True
       Para  _           -> True
       BulletList _      -> isSimpleList x
       OrderedList _ _   -> isSimpleList x
       DefinitionList _  -> isSimpleList x
       _                 -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _      -> isSimpleList y
       OrderedList _ _   -> isSimpleList y
       DefinitionList _  -> isSimpleList y
       _                 -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

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
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToPukiWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToPukiWiki opts celltype alignment item)
            alignStrings cols'
  return $ "<tr class=\"" ++ rowclass ++ "\">\n" ++ unlines cols'' ++ "</tr>"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToPukiWiki :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToPukiWiki opts celltype align' item = do
  let mkcell x = "<" ++ celltype ++ " align=\"" ++ align' ++ "\">" ++
                    x ++ "</" ++ celltype ++ ">"
  contents <- blockListToPukiWiki opts item
  return $ mkcell contents

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
  return $ "''" ++ contents ++ "''"

inlineToPukiWiki opts (Strong lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "'''" ++ contents ++ "'''"

inlineToPukiWiki opts (Strikeout lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "<s>" ++ contents ++ "</s>"

inlineToPukiWiki opts (Superscript lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "<sup>" ++ contents ++ "</sup>"

inlineToPukiWiki opts (Subscript lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "<sub>" ++ contents ++ "</sub>"

inlineToPukiWiki opts (SmallCaps lst) = inlineListToPukiWiki opts lst

inlineToPukiWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToPukiWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToPukiWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToPukiWiki opts (Cite _  lst) = inlineListToPukiWiki opts lst

inlineToPukiWiki _ (Code _ str) =
  return $ "<tt>" ++ (escapeString str) ++ "</tt>"

inlineToPukiWiki _ (Str str) = return $ escapeString str

inlineToPukiWiki _ (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                                 -- note:  str should NOT be escaped

inlineToPukiWiki _ (RawInline "pukiWiki" str) = return str
inlineToPukiWiki _ (RawInline "html" str) = return str
inlineToPukiWiki _ (RawInline _ _) = return ""

inlineToPukiWiki _ (LineBreak) = return "<br />\n"

inlineToPukiWiki _ Space = return " "

inlineToPukiWiki opts (Link txt (src, _)) = do
  label <- inlineListToPukiWiki opts txt
  case txt of
     [Code _ s] | s == src -> return src
     _  -> if isURI src
              then return $ "[" ++ src ++ " " ++ label ++ "]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToPukiWiki opts (Image alt (source, tit)) = do
  alt' <- inlineListToPukiWiki opts alt
  let txt = if (null tit)
               then if null alt
                       then ""
                       else "|" ++ alt'
               else "|" ++ tit
  return $ "[[Image:" ++ source ++ txt ++ "]]"

inlineToPukiWiki opts (Note contents) = do
  contents' <- blockListToPukiWiki opts contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" ++ contents' ++ "</ref>"
  -- note - may not work for notes with multiple blocks
