{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Forester.Forester
       ( Tree(..)
       , ForesterMeta(..)
       , emptyMeta, ul, ol, transclude, emptyTree
       , ForesterContent
       , ForesterContent'(..) ) where

import Data.Text (Text)
import Agda.Syntax.Common.Pretty
import qualified Data.Text as T

data Tree = Tree
    { treeId :: Maybe Text -- ^ Nothing for anonymouse subtrees
    , treeMeta   :: ForesterMeta
    , treeContent :: ForesterContent
    }

emptyTree :: Tree
emptyTree = Tree
    { treeId = Nothing
    , treeMeta = emptyMeta
    , treeContent = []
    }

data ForesterMeta = Meta 
    { title   :: Maybe Text
    , author  :: [Text]
    , taxon   :: Maybe Text
    , date    :: Maybe Text
    , meta    :: [(Text,Text)]  
    }

emptyMeta :: ForesterMeta
emptyMeta = Meta{
    title = Nothing,
    author = [],
    taxon = Nothing,
    date = Nothing,
    meta  = []
}

data ForesterContent'
    = Raw Text
    | Command Text ForesterContent
    | Subtree Tree
    | Link Text (Maybe Text)
    
ul, ol :: [ForesterContent] -> ForesterContent'
ul cs = Command "ul" $ fmap (Command "li") cs
ol cs = Command "ol" $ fmap (Command "li") cs

transclude :: Text -> ForesterContent'
transclude = Command "transclude" . (:[]) . Raw 


type ForesterContent = [ForesterContent']

instance Pretty ForesterContent' where
  pretty (Raw t) = pretty t
  pretty (Command t c) 
    = char '\\' <> pretty t
        <> braces (vcat (pretty <$> c))
  pretty (Subtree tree)
   = case treeId tree of
       Just i -> text "\\subtree" <> brackets (pretty i) <> braces (nest 2 $ pretty tree)
       Nothing -> text "\\subtree" <> braces (nest 2 $ pretty tree)
  pretty (Link n _) = brackets . brackets . pretty $ n



metaToContent :: ForesterMeta -> ForesterContent
metaToContent (Meta title auth txn dt mt) = 
  let (<$:>) :: Maybe a -> [a] -> [a]
      Just x <$:> ys = x : ys
      Nothing <$:> ys = ys
  in
   (Command "title" . (:[]) . Raw <$> title) <$:>
   (Command "author" . (:[]) . Raw <$> auth) ++
   (Command "taxon" . (:[]) . Raw <$> txn) <$:> []
   -- (Command "meta" . (:[]) . Raw <$> mt)

instance Pretty Tree where
  -- pretty :: Tree -> Doc
  pretty :: Tree -> Doc
  pretty (Tree _ meta content) 
   = vcat (pretty <$> metaToContent meta) $+$
     vcat (pretty <$> content)