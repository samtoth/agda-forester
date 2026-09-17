{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
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
import Data.IORef

import GHC.Generics
import Control.DeepSeq

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


data ForesterOpts = Opts
  { optsEnabled :: Bool
  , optsTreeDir :: FilePath
  , optsHtmlDir :: FilePath
  , optsHtmlLinkRoot :: FilePath
  , optsHtmlCssPath :: FilePath
  , optsForestRoot :: FilePath
  , optsEnableBacklinks :: Bool
  , optsGenIndexTrees :: Bool
  -- , optsStructured :: FStructured
  } deriving (Generic, NFData)

defaultOps :: ForesterOpts
defaultOps = Opts
  { optsEnabled = False
  , optsTreeDir = "trees"
  , optsHtmlDir = "assets/html"
  , optsHtmlLinkRoot = "/html/"
  , optsHtmlCssPath = "Agda.css"
  , optsForestRoot = "/"
  , optsEnableBacklinks = True
  , optsGenIndexTrees = True
  -- , optsStructured = FSNone
  }

data ForesterIdent = ForesterIdent

data CompEnv = CompEnv
  { compileEnvOpts     :: ForesterOpts
  , compileForestData  :: IORef (HashMap Text FInfo)
  , compileMods        :: IORef ModuleData
  }

data ModuleEnv = ModuleEnv
  { modEnvCompileEnv :: CompEnv
  , modEnvName       :: TopLevelModuleName
  }

data ForesterModule = ForesterModule
  {
  }

data CodeGenEnv = CodeGenEnv
  { cgOpts :: ForesterOpts
  , cgMods :: ModuleData
  , cgForestData :: HashMap Text FInfo
  }
