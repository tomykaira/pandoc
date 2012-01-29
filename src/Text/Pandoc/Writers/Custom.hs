{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Data.List ( intersect, intercalate )
import Scripting.Lua (LuaState, StackValue, callfunc)
import qualified Scripting.Lua as Lua
import Data.Maybe (fromJust)

getList :: StackValue a => LuaState -> Int -> IO [a]
getList lua i' = do
  continue <- Lua.next lua i'
  if continue
     then do
       next <- Lua.peek lua (-1)
       Lua.pop lua 1
       x <- maybe (fail "peek returned Nothing") return next
       rest <- getList lua i'
       return (x : rest)
     else return []

instance StackValue a => StackValue [a] where
  push lua xs = do
    Lua.createtable lua (length xs + 1) 0
    let addValue (i, x) = Lua.push lua x >> Lua.rawseti lua (-2) i
    mapM_ addValue $ zip [1..] xs
  peek lua i = do
    top <- Lua.gettop lua
    let i' = if i < 0 then top + i + 1 else i
    Lua.pushnil lua
    lst <- getList lua i'
    Lua.pop lua 1
    return (Just lst)
  valuetype _ = Lua.TTABLE

instance (StackValue a, StackValue b) => StackValue [(a,b)] where
  push lua xs = do
    Lua.createtable lua (length xs + 1) 0
    let addValue (k, v) = Lua.push lua k >> Lua.push lua v >>
                          Lua.rawset lua (-3)
    mapM_ addValue xs
  peek lua i = undefined -- not needed for our purposes
  valuetype _ = Lua.TTABLE

-- | Convert Pandoc to custom markup.
writeCustom :: WriterOptions -> String -> Pandoc -> IO String
writeCustom opts luaScript (Pandoc _ blocks) = do
  lua <- Lua.newstate
  Lua.openlibs lua
  Lua.loadstring lua luaScript "custom"
  Lua.call lua 0 0
  body <- blockListToCustom lua blocks
  Lua.close lua
  return body

-- | Convert Pandoc block element to Custom.
blockToCustom :: LuaState      -- ^ Lua state
              -> Block         -- ^ Block element
              -> IO String

blockToCustom _ Null = return ""

blockToCustom lua (Plain inlines) =
  inlineListToCustom lua inlines

blockToCustom lua (Para inlines) =  -- TODO fornow
  inlineListToCustom lua inlines

{-
blockToCustom opts (Para [Image txt (src,tit)]) =
  capt <- inlineListToCustom opts txt
  let opt = if null txt
               then ""
               else "|alt=" ++ if null tit then capt else tit ++
                    "|caption " ++ capt
  return "CAPTION-IMAGE"

blockToCustom opts (Para inlines) = do
  useTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToCustom opts inlines
  return $ if useTags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null listLevel then "\n" else ""

blockToCustom _ (RawBlock "mediawiki" str) = return str
blockToCustom _ (RawBlock "html" str) = return str
blockToCustom _ (RawBlock _ _) = return ""

blockToCustom _ HorizontalRule = return "\n-----\n"

blockToCustom opts (Header level inlines) = do
  contents <- inlineListToCustom opts inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToCustom _ (CodeBlock (_,classes,_) str) = do
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl", 
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  let (beg, end) = if null at
                      then ("<pre" ++ if null classes then ">" else " class=\"" ++ unwords classes ++ "\">", "</pre>")
                      else ("<source lang=\"" ++ head at ++ "\">", "</source>")
  return $ beg ++ escapeString str ++ end

blockToCustom opts (BlockQuote blocks) = do
  contents <- blockListToCustom opts blocks
  return $ "<blockquote>" ++ contents ++ "</blockquote>" 

blockToCustom opts (Table capt aligns widths headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToCustom opts capt
                      return $ "<caption>" ++ c ++ "</caption>\n"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then ""
                   else unlines $ map
                         (\w -> "<col width=\"" ++ percent w ++ "\" />") widths
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToCustom opts alignStrings 0 headers
                 return $ "<thead>\n" ++ hs ++ "\n</thead>\n"
  body' <- zipWithM (tableRowToCustom opts alignStrings) [1..] rows'
  return $ "<table>\n" ++ captionDoc ++ coltags ++ head' ++
            "<tbody>\n" ++ unlines body' ++ "</tbody>\n</table>\n"

blockToCustom opts x@(BulletList items) = do
  oldUseTags <- get >>= return . stUseTags
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToCustom opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "*" }
        contents <- mapM (listItemToCustom opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ "\n"

blockToCustom opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToCustom opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "#" }
        contents <- mapM (listItemToCustom opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ "\n"

blockToCustom opts x@(DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (definitionListItemToCustom opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ ";" }
        contents <- mapM (definitionListItemToCustom opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ "\n"

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

-- | Convert bullet or ordered list item (list of blocks) to Custom.
listItemToCustom :: LuaState -> [Block] -> State WriterState String
listItemToCustom opts items = do
  contents <- blockListToCustom opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to Custom.
definitionListItemToCustom :: LuaState
                             -> ([Inline],[[Block]]) 
                             -> State WriterState String
definitionListItemToCustom opts (label, items) = do
  labelText <- inlineListToCustom opts label
  contents <- mapM (blockListToCustom opts) items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<dt>" ++ labelText ++ "</dt>\n" ++
           (intercalate "\n" $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ labelText ++ "\n" ++
           (intercalate "\n" $ map (\d -> init marker ++ ": " ++ d) contents)

-- Auxiliary functions for tables:

tableRowToCustom :: LuaState
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToCustom opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith 
            (\alignment item -> tableItemToCustom opts celltype alignment item) 
            alignStrings cols'
  return $ "<tr class=\"" ++ rowclass ++ "\">\n" ++ unlines cols'' ++ "</tr>"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToCustom :: LuaState
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToCustom opts celltype align' item = do
  let mkcell x = "<" ++ celltype ++ " align=\"" ++ align' ++ "\">" ++
                    x ++ "</" ++ celltype ++ ">"
  contents <- blockListToCustom opts item
  return $ mkcell contents
-}

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: LuaState -- ^ Options
                  -> [Block]       -- ^ List of block elements
                  -> IO String
blockListToCustom lua = fmap unlines . mapM (blockToCustom lua)

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: LuaState -> [Inline] -> IO String
inlineListToCustom lua lst = do
  xs <- mapM (inlineToCustom lua) lst
  return $ concat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: LuaState -> Inline -> IO String

inlineToCustom lua (Str str) =
  callfunc lua "writer.str" str

inlineToCustom lua Space =
  callfunc lua "writer.space"

inlineToCustom lua (Emph lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.emph" x

inlineToCustom lua (Strong lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.strong" x

inlineToCustom lua (Strikeout lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.strikeout" x

inlineToCustom lua (Superscript lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.superscript" x

inlineToCustom lua (Subscript lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.subscript" x

inlineToCustom lua (SmallCaps lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.smallcaps" x

inlineToCustom lua (Quoted SingleQuote lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.singlequoted" x

inlineToCustom lua (Quoted DoubleQuote lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.doublequoted" x

inlineToCustom lua (Cite _  lst) = do
  x <- inlineListToCustom lua lst
  callfunc lua "writer.cite" x

inlineToCustom lua (Code (id,classes,keyvals) str) = do
  callfunc lua "writer.code" str id classes keyvals

{-

inlineToCustom _ (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                                 -- note:  str should NOT be escaped

inlineToCustom _ (RawInline "mediawiki" str) = return str 
inlineToCustom _ (RawInline "html" str) = return str 
inlineToCustom _ (RawInline _ _) = return ""

inlineToCustom _ (LineBreak) = return "<br />\n"


inlineToCustom lua (Link txt (src, _)) = do
  label <- inlineListToCustom lua txt
  case txt of
     [Code _ s] | s == src -> return src
     _  -> if isURI src
              then return $ "[" ++ src ++ " " ++ label ++ "]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToCustom lua (Image alt (source, tit)) = do
  alt' <- inlineListToCustom lua alt
  let txt = if (null tit)
               then if null alt
                       then ""
                       else "|" ++ alt'
               else "|" ++ tit
  return $ "[[Image:" ++ source ++ txt ++ "]]"

inlineToCustom lua (Note contents) = do 
  contents' <- blockListToCustom lua contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" ++ contents' ++ "</ref>"
  -- note - may not work for notes with multiple blocks
-}
