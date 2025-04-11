{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Forester.Structured where

import Agda.Compiler.Backend
import Agda.Syntax.Scope.Base (DataOrRecordModule(..), Scope (..))
import Agda.Compiler.Common ( curIF )
import Agda.Syntax.Common.Pretty

import Agda.Utils.Monad (forM, join, forM_)
import Agda.Utils.Tuple ((/\))
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (fromJust)
import qualified Agda.Utils.IO.UTF8 as UTF8


import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Text (Text, pack, unpack)
import Data.List.Split (splitOn)
import Control.Monad.IO.Class (liftIO)
import System.FilePath
import qualified Data.Text.Lazy as T
import GHC.Generics
import Control.DeepSeq

import Forester.Base
import Forester.Forester
import Data.HashMap.Strict (HashMap)
import Agda.Syntax.Common (FileType)

data FStructured
  = FSNone
  | FSSemi
  | FSFull
  deriving (Generic, NFData)

data Mod
  = SubMods ModuleName [TopLevelModuleName] [TokenInfo] [Mod] [(ForesterDef, [TokenInfo])]
  | DataMod ModuleName [TokenInfo] [ForesterDef]
  | RecordMod ModuleName [TokenInfo] [ForesterDef]

instance Pretty Mod where
  pretty (SubMods mn _ _ smods defs) 
    = pretty mn <> ": " <> vsep [pretty smods, pretty (foresterDefId . fst <$> defs)]
  pretty (DataMod nm _ defs) = "Data" <> parens (pretty nm) <> ": " <> pretty (foresterDefId <$> defs)
  pretty (RecordMod nm _ defs) = "Record" <> parens (pretty nm) <> ": " <> pretty (foresterDefId <$> defs)

createModTree :: TopLevelModuleName -> ModuleName -> [ForesterDef] -> Sections -> [(Maybe String, [TokenInfo])] -> TCMT IO Mod
-- createModTree tlname _ [] _ _ = return $ SubMods (MName []) [] [] [] []
createModTree tlname mn@(MName nms) defs secs code
  = do
    mi <- curIF
    let submods = Map.keys secs
    rest <- forM (filter (immChildOf mn) submods) $ \nm -> do
      case scopeDatatypeModule =<< nm `Map.lookup` iScope mi of
        Just IsDataModule -> return . Just . DataMod nm 
                                (join . fmap snd . filter (stringStarts (render $ pretty nm). fst) $ code)
                              $ (filter (defStarts nm) defs)
        Just IsRecordModule -> return . Just . RecordMod nm 
                                (join . fmap snd . filter (stringStarts (render $ pretty nm) . fst) $ code)
                              $ (filter (defStarts nm) defs)
        Nothing ->  Just <$> createModTree 
                              tlname
                              nm
                              (filter (defStarts nm) defs)
                              (Map.filterWithKey (const . nameStarts nm) secs)
                              code
    let tlDefsWithCode :: [(ForesterDef, [TokenInfo])]
        tlDefsWithCode = uncurry zip . (id /\ fmap getCode) . filter (defIs mn) $ defs

    -- liftIO $ putStrLn $ join $ fTok <$> (fromJust $ flip lookup code $
    --                 case moduleTail of
    --                     [] -> Nothing
    --                     '.':xs -> Just xs
    --                     _ -> __IMPOSSIBLE__)

    let preamble = maybe [] id $ flip lookup code $
                    case moduleTail of
                        [] -> Nothing
                        '.':xs -> Just xs
                        _ -> __IMPOSSIBLE__
    

    imps <- fmap fst . iImportedModules <$> curIF
    return $ SubMods mn imps preamble (maybe [] (:[]) =<< rest) tlDefsWithCode
  where
    nameStarts :: ModuleName -> ModuleName -> Bool
    nameStarts (MName mn) (MName mn') = List.isPrefixOf mn mn'

    qnameList :: QName -> [Name]
    qnameList (QName (MName xs) y) = xs ++ [y]

    moduleTail :: String
    moduleTail = fromJust $ List.stripPrefix (render . pretty $ tlname) (render . pretty $ mn)

    getCode :: ForesterDef -> [TokenInfo]
    getCode = maybe [] id . flip lookup code . Just .
              tail.((moduleTail ++ ".") ++) . last .
              splitOn "." .unpack.foresterDefId 

    stringStarts :: String -> Maybe String -> Bool
    stringStarts s Nothing = False
    stringStarts s (Just s') = List.isPrefixOf s s'

    defStarts :: ModuleName -> ForesterDef -> Bool
    defStarts mn = nameStarts mn . qnameModule . defName . foresterDefDef 

    immChildOf :: ModuleName -> ModuleName -> Bool
    immChildOf (MName ys) (MName (xs)) = take (length ys) xs == ys && (length xs == length ys + 1)

    defIs :: ModuleName -> ForesterDef -> Bool
    defIs nm = (== nm) . qnameModule . defName . foresterDefDef 



realiseModTree :: HashMap Text Text -> HashMap TopLevelModuleName FileType -> FilePath -> Mod -> TCMT IO Tree
realiseModTree _ _ root (DataMod   mname toks defs) = return emptyTree {treeId = Just . pack.render . pretty $ mname}
realiseModTree _ _ root (RecordMod mname toks defs) = return emptyTree {treeId = Just . pack.render . pretty $ mname}
realiseModTree ds hm root (SubMods mname imps preamble smods defs) = do
  let defTrees :: [(Text, Tree)]
      defTrees = ((foresterDefId . fst) /\ uncurry (definitionTree ds hm)) <$> defs
  forM_ defTrees $ \(fp, t) -> do
    liftIO $ UTF8.writeTextToFile (root </> unpack fp <.> "tree") $ T.pack . render . pretty $ t

  subtrees <- mapM (realiseModTree ds hm root) smods

  let tlTree = createTree ds hm mname imps preamble (fmap snd defTrees ++ subtrees)

  liftIO $ UTF8.writeTextToFile (root </> render (pretty mname) <.> "tree") $ T.pack . render . pretty $ tlTree

  return tlTree
