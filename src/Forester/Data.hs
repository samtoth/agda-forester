{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Forester.Data where

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor, (.=))
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Position
import Agda.Syntax.Internal
import Agda.Syntax.Common (FileType, TopLevelModuleName')
import Agda.Syntax.TopLevelModuleName
import Agda.TypeChecking.Serialise.Base

import Agda.Interaction.JSON hiding (text)
import qualified Data.Aeson.Encoding as JSON (text)

import Data.HashMap.Strict (HashMap)

import Data.Text (Text)
import qualified Data.Text as T

data FInfo = FInfo
  { fqname :: QName
  , ftId   :: Maybe T.Text
  , fty    :: Type
  }

instance EmbPrj FInfo where
  icod_ (FInfo fnm ftid fty) = icodeN' FInfo fnm ftid fty
  value = valueN FInfo

type ModuleData = HashMap T.Text (FileType, [IntervalTree])

-- interval trees (mapping from filepos -> subtree id)

data IntervalTree = IntTree
    { itId        :: T.Text
    , itStartPos  :: (Int, Int)
    , itEndPos    :: (Int, Int)
    , itChildren  :: [IntervalTree]
    }

instance FromJSON IntervalTree where
  parseJSON = withObject "IntervalTree" $ \v ->
      IntTree <$> v .: "name" <*> v .: "start_pos" <*> v .: "end_pos" <*> v .: "children" 

instance ToJSON IntervalTree where
  toJSON (IntTree id sp ep cs) = object ["name" .= id, "start_pos" .= sp, "end_pos" .= ep, "children" .= cs]

instance ToJSON FileType where
instance FromJSON FileType where

getSubtree :: [IntervalTree] -> Int -> Maybe T.Text
getSubtree [] _ = Nothing
getSubtree ((IntTree tid (_,sp) (_,ep) cp):xs) os 
  = if os >= sp && os <= ep 
    then maybe (Just tid) Just $ getSubtree cp os 
    else getSubtree xs os
