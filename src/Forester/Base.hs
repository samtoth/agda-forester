{-# LANGUAGE OverloadedStrings #-}
module Forester.Base where

import Data.Monoid (Endo(..))
import Data.Foldable (toList)
import Data.List.Split
import qualified Data.Text.Lazy as T
import Data.Text (Text, pack, unpack)
import qualified Data.List as List
import qualified Data.IntMap as IntMap

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Syntax.Common
import Agda.Interaction.Highlighting.Precise hiding (toList)

import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Function (on)
import Agda.Utils.Maybe (fromMaybe, fromJust, isJust)
import Agda.Syntax.Common.Pretty
import Agda.Utils.Monad (join)

import Forester.Forester



data ForesterDef = ForesterDef 
    { foresterDefTree :: ForesterMeta
    , foresterDefId   :: Text
    , foresterDefDef  :: Definition 
    }

type TokenInfo =
  ( Int
  , String
  , Aspects
  )




definitionTree :: ForesterDef -> [TokenInfo] -> Tree
definitionTree def toks
  = emptyTree 
  { treeId = Just . foresterDefId $ def
  , treeMeta = foresterDefTree def
  , treeContent = 
      [ Command "import" [Raw "macros"]
      , Command "agda" [Raw . pack . mconcat $ fTok <$> toks]
        -- Raw $ pack ("html" </> (render.pretty$tlname) <.> "html#" <> unpack (fromJust .title.foresterDefTree$def) )]
      ]
  }


createTree :: ModuleName -> [TopLevelModuleName] -> [TokenInfo] -> [Tree] -> Tree
createTree tlname iMods preamb defs
  = let meta = emptyMeta
          { title = Just . pack .render . pretty $ tlname
          , taxon = Just "agda module"
          }
        imports = Tree 
          { treeId = Nothing
          , treeMeta = emptyMeta {title = Just "imports"}
          , treeContent = 
            [ ul ((:[]) . toLink <$> iMods)
            ]
          }
        preamble = Tree
          { treeId = Nothing
          , treeMeta = emptyMeta {title = Just "preamble"}
          , treeContent =
            [ Command "agda" [Raw . pack . mconcat $ fTok <$> preamb]
            ]
          }
        content = Command "import" [Raw "macros"] : Subtree imports : Subtree preamble : fmap (transclude . fromJust . treeId) defs
          
    in Tree
    { treeId = Just . pack . render . pretty $ tlname
    , treeMeta = meta
    , treeContent = content
    }


toLink :: TopLevelModuleName -> ForesterContent'
toLink = flip Link Nothing . pack . render . pretty 


fTok :: TokenInfo -> String
fTok (pos, cont, asp) = appEndo (mconcat $ fmap (\c -> Endo (\x -> "\\" ++ c ++ "{" ++ x ++ "}")) classes) $ filterC =<< cont where

  filterC :: Char -> String
  filterC '(' = "\\lpar{}"
  filterC ')' = "\\rpar{}"
  filterC '{' = "\\lbrace{}"
  filterC '}' = "\\rbrace{}"
  filterC s = s:[]

  classes = concat
      [ concatMap noteClasses (note asp)
      , otherAspectClasses (toList $ otherAspects asp)
      , concatMap aspectClasses (aspect asp)
      ]

  aspectClasses (Name mKind op) = kindClass ++ opClass
    where
    kindClass = toList $ fmap showKind mKind

    showKind (Constructor Inductive)   = "InductiveConstructor"
    showKind (Constructor CoInductive) = "CoinductiveConstructor"
    showKind k                         = show k

    opClass = ["Operator" | op]
  aspectClasses a = [show a]


  otherAspectClasses = map show

  -- Notes are not included.
  noteClasses _s = []



splitDef :: [TokenInfo]
          -> [(Maybe String, [TokenInfo])]
splitDef = fmap help
         . split (keepDelimsL $ whenElt (\(a,_) -> isJust a))
         . fmap (\xs@((_,_,x):_) -> (maybe Nothing (defSiteAnchor) (definitionSite x) , xs)) 
         . split (keepDelimsL $ whenElt here)
 where 
  help :: [(Maybe String, [TokenInfo])] -> (Maybe String, [TokenInfo])
  help xs = ((fst . head) xs, join (fmap snd xs) )

  -- Are we at the definition site now?
  here            :: TokenInfo -> Bool
  here (_, s, mi)  = maybe False defSiteHere mDefinitionSite && isJust mDefSiteAnchor where
    mDefinitionSite :: Maybe DefinitionSite
    mDefinitionSite = definitionSite mi

    mDefSiteAnchor  :: Maybe String
    mDefSiteAnchor  = maybe __IMPOSSIBLE__ defSiteAnchor mDefinitionSite
  


-- | Constructs token stream ready to print.

tokenStream
  :: T.Text             -- ^ The contents of the module.
  -> HighlightingInfo -- ^ Highlighting information.
  -> [TokenInfo]
tokenStream contents info =
  map (\cs -> case cs of
          (mi, (pos, _)) : _ ->
            (pos, map (snd . snd) cs, fromMaybe mempty mi)
          [] -> __IMPOSSIBLE__) $
  List.groupBy ((==) `on` fst) $
  zipWith (\pos c -> (IntMap.lookup pos infoMap, (pos, c))) [1..] (T.unpack contents)
  where
  infoMap = toMap info