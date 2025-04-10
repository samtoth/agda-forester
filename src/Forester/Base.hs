module Forester.Base where

import Data.Monoid (Endo(..))
import Data.Foldable (toList)
import qualified Data.Text.Lazy as T
import Data.Text (Text, pack, unpack)
import qualified Data.List as List
import qualified Data.IntMap as IntMap

import Agda.Syntax.Common.Aspect
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Function (on)
import Agda.Interaction.Highlighting.Precise (HighlightingInfo, toMap)
import Agda.Utils.Maybe (fromMaybe)
import Agda.Compiler.Backend (Definition)

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