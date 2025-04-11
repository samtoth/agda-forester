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
import Agda.Utils.Function (on, applyUnless)
import Agda.Utils.Maybe (fromMaybe, fromJust, isJust)
import Agda.Syntax.Common.Pretty
import Agda.Utils.Monad (join)

import Forester.Forester
import qualified Network.URI.Encode
import System.FilePath
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM



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

aspectsTokenInfo :: TokenInfo -> Aspects
aspectsTokenInfo (_,_,a) = a

codeTree :: HashMap Text Text -> HashMap TopLevelModuleName FileType -> [TokenInfo] -> String
codeTree ds hm = join . fmap mkTree . splitByMarkup where

  splitByMarkup :: [TokenInfo] -> [[TokenInfo]]
  splitByMarkup = splitWhen $ (== Just Markup) . aspect . aspectsTokenInfo

  -- The assumption here and in mkOrg is that Background tokens and Agda tokens are always
  -- separated by Markup tokens, so these runs only contain one kind.
  mkTree :: [TokenInfo] -> String
  mkTree tokens = if containsCode then formatCode else formatNonCode
    where
      containsCode = any ((/= Just Background) . aspect . aspectsTokenInfo) tokens

      formatCode = render . pretty $ Command "agda" [Raw . pack . mconcat $ fTok ds hm <$> tokens] -- $ mconcat $ backgroundOrAgdaToHtml <$> tokens
      formatNonCode = mconcat $ backgroundOrAgdaToTree <$> tokens

  backgroundOrAgdaToTree :: TokenInfo -> String
  backgroundOrAgdaToTree token@(_, s, mi) = case aspect mi of
    Just Background -> s
    Just Markup     -> __IMPOSSIBLE__
    _               -> fTok ds hm token

definitionTree :: HashMap Text Text -> HashMap TopLevelModuleName FileType -> ForesterDef -> [TokenInfo] -> Tree
definitionTree ds hm def toks
  = emptyTree 
  { treeId = Just . foresterDefId $ def
  , treeMeta = foresterDefTree def
  , treeContent = 
      [ Command "import" [Raw "macros"]
      , Command "agda" [Raw . pack . mconcat $ fTok ds hm <$> toks]
        -- Raw $ pack ("html" </> (render.pretty$tlname) <.> "html#" <> unpack (fromJust .title.foresterDefTree$def) )]
      ]
  }


createTree :: HashMap Text Text -> HashMap TopLevelModuleName FileType -> ModuleName -> [TopLevelModuleName] -> [TokenInfo] -> [Tree] -> Tree
createTree ds hm tlname iMods preamb defs
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
            [ Command "agda" [Raw . pack . mconcat $ fTok ds hm <$> preamb]
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

-- | Converts module names to the corresponding HTML file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext

fTok :: HashMap Text Text -> HashMap TopLevelModuleName FileType -> TokenInfo -> String
fTok defSrc hm (pos, cont, asp) = appEndo (mconcat $ fmap (\c -> Endo (\x -> "\\" ++ c ++ "{" ++ x ++ "}")) classes <> annotate) $ filterC =<< cont where

  filterC :: Char -> String
  filterC '(' = "\\lpar{}"
  filterC ')' = "\\rpar{}"
  filterC '{' = "\\lbrace{}"
  filterC '}' = "\\rbrace{}"
  filterC '[' = "\\lbrack{}"
  filterC ']' = "\\rbrack{}"
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

  link :: DefinitionSite -> [String -> String]
  link (DefinitionSite m defPos _here aName) = case HM.lookup m hm of
      Just AgdaFileType -> 
        -- If the definition site points to the top of a file,
        -- we drop the anchor part and just link to the file.
        let l = applyUnless (defPos <= 1)
                    (++ "#" ++
                    (show defPos))
                    -- Network.URI.Encode.encode (fromMaybe (show defPos) aName)) -- Named links disabled
                    (Network.URI.Encode.encode $ modToFile m "html")
        in [\s -> "[" <> s <> "]" <> "(" <> "html/" <> l <> ")"]
      _ -> case flip HM.lookup defSrc . pack =<< aName of
        Just a  -> [\s -> "[" <> s <> "]" <> "(" <> unpack a <> ")"]
        Nothing -> case flip HM.lookup defSrc.pack.render.pretty $ m of
            Just a -> [\s -> "[" <> s <> "]" <> "(" <> unpack a <> ")"]
            Nothing -> []


  annotate :: [Endo String]
  annotate = join $ (fmap Endo . link <$> toList (definitionSite asp))
--   annotate s = "[" <> (join . toList $ (link <$> definitionSite asp)) <> "]" <> "(" <> s <> ")"




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