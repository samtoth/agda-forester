{-# LANGUAGE OverloadedStrings #-}
module Forester.Html where

import Prelude hiding ((!!))
import qualified Data.Text.Lazy as T
import System.FilePath
import Data.List.Split
import Data.Foldable (toList)


import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Syntax.Common
import Agda.Interaction.Highlighting.Precise hiding (toList)
import Agda.Syntax.Common.Pretty

import Agda.Utils.Impossible
import qualified Agda.Utils.List1 as List1
import Agda.Utils.Monad (guard)
import Agda.Utils.Maybe (fromMaybe, isJust)
import Agda.Utils.Function (applyUnless, applyWhen)

import qualified Network.URI.Encode

import Text.Blaze.Html5
    ( preEscapedToHtml
    , toHtml
    , stringValue
    , Html
    , (!)
    , Attribute
    )
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Html.Renderer.Text (renderHtml)


import Forester.Base


-- | Internal type bundling the information related to a module source file

data HtmlInputSourceFile = HtmlInputSourceFile
  { _srcFileModuleName :: TopLevelModuleName
  , _srcFileType :: FileType
  -- ^ Source file type
  , _srcFileText :: T.Text
  -- ^ Source text
  , _srcFileHighlightInfo :: HighlightingInfo
  -- ^ Highlighting info
  }
  deriving Show


srcFileOfInterface :: TopLevelModuleName -> Interface -> HtmlInputSourceFile
srcFileOfInterface m i = HtmlInputSourceFile m (iFileType i) (iSource i) (iHighlighting i)

-- | Converts module names to the corresponding HTML file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext


-- | Split a token stream into definitions

splitTokens :: [TokenInfo] -> [[TokenInfo]]
splitTokens = splitWhen $ (== Just Background) . aspect . (\(_,_,a) -> a)


(!!) :: Html -> [Attribute] -> Html
h !! as = h ! mconcat as
-- | Constructs the web page, including headers.

page
  :: Bool                  -- ^ Whether to reserve literate
  -> TopLevelModuleName  -- ^ Module to be highlighted.
  -> Html
  -> Html
page htmlHighlight modName pageContent =
  if htmlHighlight
    then pageContent
    else Html5.docTypeHtml $ hdr <> rest
  where

    hdr = Html5.head $ mconcat
      [ Html5.meta !! [ Attr.charset "utf-8" ]
      , Html5.title (toHtml . render $ pretty modName)
      , Html5.link !! [ Attr.rel "stylesheet"
                      , Attr.href $ stringValue "Agda.css"
                      ]
      
      ]

    rest = Html5.body $ (Html5.pre ! Attr.class_ "Agda") pageContent

-- | Constructs the HTML displaying the code.




code :: Bool     -- ^ Whether to generate non-code contents as-is
     -> FileType -- ^ Source file type
     -> [TokenInfo]
     -> Html
code onlyCode fileType = mconcat . if onlyCode
  then case fileType of
         -- Explicitly written all cases, so people
         -- get compile error when adding new file types
         -- when they forget to modify the code here
         RstFileType   -> __IMPOSSIBLE__
         MdFileType    -> __IMPOSSIBLE__
         AgdaFileType  -> map mkHtml
         OrgFileType   -> __IMPOSSIBLE__
         TreeFileType  -> map mkTree . splitByMarkup
         -- Two useless cases, probably will never used by anyone
         TexFileType   -> __IMPOSSIBLE__
         TypstFileType -> __IMPOSSIBLE__
  else map mkHtml
  where
  trd (_, _, a) = a

  splitByMarkup :: [TokenInfo] -> [[TokenInfo]]
  splitByMarkup = splitWhen $ (== Just Markup) . aspect . trd

  -- The assumption here and in mkOrg is that Background tokens and Agda tokens are always
  -- separated by Markup tokens, so these runs only contain one kind.
  mkTree :: [TokenInfo] -> Html
  mkTree tokens = if containsCode then formatCode else formatNonCode
    where
      containsCode = any ((/= Just Background) . aspect . trd) tokens

      formatCode = Html5.pre ! Attr.class_ "Agda" $ mconcat $ backgroundOrAgdaToHtml <$> tokens
      formatNonCode = mconcat $ backgroundOrAgdaToHtml <$> tokens

  mkHtml :: TokenInfo -> Html
  mkHtml (pos, s, mi) =
    -- Andreas, 2017-06-16, issue #2605:
    -- Do not create anchors for whitespace.
    applyUnless (mi == mempty) (annotate pos mi) $ toHtml $ List1.toList s

  backgroundOrAgdaToHtml :: TokenInfo -> Html
  backgroundOrAgdaToHtml token@(_, s, mi) = case aspect mi of
    Just Background -> preEscapedToHtml $ List1.toList s
    Just Markup     -> __IMPOSSIBLE__
    _               -> mkHtml token


  -- Put anchors that enable referencing that token.
  -- We put a fail safe numeric anchor (file position) for internal references
  -- (issue #2756), as well as a heuristic name anchor for external references
  -- (issue #2604).
  annotate :: Int -> Aspects -> Html -> Html
  annotate pos mi =
    applyWhen hereAnchor (anchorage nameAttributes mempty <>) . anchorage posAttributes
    where
    -- Warp an anchor (<A> tag) with the given attributes around some HTML.
    anchorage :: [Attribute] -> Html -> Html
    anchorage attrs html = Html5.a html !! attrs

    -- File position anchor (unique, reliable).
    posAttributes :: [Attribute]
    posAttributes = concat
      [ [Attr.id $ stringValue $ show pos ]
      , toList $ link <$> definitionSite mi
      , Attr.class_ (stringValue $ unwords classes) <$ guard (not $ null classes)
      ]

    -- Named anchor (not reliable, but useful in the general case for outside refs).
    nameAttributes :: [Attribute]
    nameAttributes = [ Attr.id $ stringValue $ fromMaybe __IMPOSSIBLE__ $ mDefSiteAnchor ]

    classes = concat
      [ concatMap noteClasses (note mi)
      , otherAspectClasses (toList $ otherAspects mi)
      , concatMap aspectClasses (aspect mi)
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

    -- Should we output a named anchor?
    -- Only if we are at the definition site now (@here@)
    -- and such a pretty named anchor exists (see 'defSiteAnchor').
    hereAnchor      :: Bool
    hereAnchor      = here && isJust mDefSiteAnchor

    mDefinitionSite :: Maybe DefinitionSite
    mDefinitionSite = definitionSite mi

    -- Are we at the definition site now?
    here            :: Bool
    here            = maybe False defSiteHere mDefinitionSite

    mDefSiteAnchor  :: Maybe String
    mDefSiteAnchor  = maybe __IMPOSSIBLE__ defSiteAnchor mDefinitionSite

    link (DefinitionSite m defPos _here _aName) = Attr.href $ stringValue $
      -- If the definition site points to the top of a file,
      -- we drop the anchor part and just link to the file.
      applyUnless (defPos <= 1)
        (++ "#" ++
         Network.URI.Encode.encode (show defPos))
         -- Network.URI.Encode.encode (fromMaybe (show defPos) aName)) -- Named links disabled
        (Network.URI.Encode.encode $ modToFile m "html")