module Forester
       ( Tree(..)
       , ForesterMeta(..)
       , ForesterContent
       , ForesterContent'(..)) where

import Data.Text (Text)

data Tree = Tree
    { treeId :: Text
    , treeSrcLoc :: FilePath
    , treeMeta   :: ForesterMeta
    , treeContent :: ForesterContent
    }

data ForesterMeta = Meta 
    { title   :: Text
    , author  :: [Text]
    , taxon   :: Text
    , meta    :: [(Text,Text)]  
    }

data ForesterContent'
    = Raw Text
    | Command Text ForesterContent
    | Latex
    
type ForesterContent = [ForesterContent']