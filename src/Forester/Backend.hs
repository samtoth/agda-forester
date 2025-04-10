
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Forester.Backend
  (foresterBackend) where

import Prelude hiding ((!!))

import Control.Monad.IO.Class


import Data.Foldable (toList, concatMap)
import Data.HashMap.Strict (HashMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.List.Split (splitWhen, whenElt, split, keepDelimsR, keepDelimsL, splitOn, startsWith, onSublist, dropDelims)
import Data.IORef
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Ts
import qualified Data.Text.Lazy as T

import Control.DeepSeq
import GHC.Generics

import System.FilePath

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Compiler.Common

-- import qualified Agda.Syntax.Concrete.Generic as Con
-- import qualified Agda.Syntax.Internal.Generic as I
-- import qualified Agda.Syntax.Abstract.Views as A
-- import qualified Agda.Syntax.Common.Aspect as Asp
-- import qualified Agda.Syntax.Scope.Base as Scope
-- import qualified Agda.Syntax.Concrete as Con
-- import qualified Agda.Syntax.Internal as I
-- import qualified Agda.Syntax.Abstract as A hiding (Name)
-- import qualified Agda.Syntax.Concrete as C


import Agda.Syntax.Common.Pretty
import Agda.Syntax.Common

import qualified Network.URI.Encode
import Agda.Utils.Function
import Agda.Utils.List1 (String1)
import qualified Agda.Utils.List1   as List1
import qualified Agda.Utils.IO.UTF8 as UTF8

import Agda.Interaction.Options (ArgDescr(..), OptDescr(..), Flag)

import Forester.Forester
import Forester.Base
import Forester.Structured

import Agda.Utils.Monad (guard, unless, forM_, join, forM)
import Agda.Utils.List1 ( NonEmpty((:|)) )
import Agda.Interaction.Highlighting.Precise hiding (toList)
import qualified Data.IntMap.Strict as IntMap
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Function (on)
import Agda.Utils.Maybe (fromMaybe, fromJust, isJust)
import qualified Agda.Utils.IO.UTF8 as UFT8
import Agda.Syntax.Scope.Base

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
import Data.Bifunctor (first, second)
import Data.Monoid (Endo(..))
import Agda.Utils.Tuple ((/\))
import Agda.Compiler.Backend (Name)

data FStructured
  = Html
  | Semi
  | Full
  deriving (Generic, NFData)

data ForesterOpts = Opts
  { optsEnabled :: Bool
  , optsTreeDir :: FilePath
  , optsStructured :: FStructured
  } deriving (Generic, NFData)

defaultOps :: ForesterOpts
defaultOps = Opts 
  { optsEnabled = False
  , optsTreeDir = "trees"
  , optsStructured = Html
  }

data ForesterIdent = ForesterIdent

data CompEnv = CompEnv {
    compileEnvOpts     :: ForesterOpts
  , compileTypes       :: IORef (HashMap Text ForesterIdent)
    -- ^ Hashmap from anchor→identifier for finding types while emitting
    -- HTML, and for search after
    }

data ModuleEnv = ModuleEnv
  { modEnvCompileEnv :: CompEnv
  , modEnvName       :: TopLevelModuleName
  }

newtype ForesterModule = ForesterModule 
  { getHtmlModule :: HashMap Text ForesterIdent
  }
    

foresterBackend :: Backend
foresterBackend = Backend foresterBackend'

foresterBackend' ::  Backend' ForesterOpts CompEnv ModuleEnv ForesterModule (Maybe ForesterDef)
foresterBackend' = Backend' 
    { backendName = "Forester"
      -- ^ the name of the backend
    , backendVersion = Nothing
      -- ^ Optional version information to be printed with @--version@.
    , options = defaultOps
      -- ^ Default options
    , commandLineFlags = fFlags
      -- ^ Backend-specific command-line flags. Should at minimum contain a
      --   flag to enable the backend.
    , isEnabled = const True
      -- ^ Unless the backend has been enabled, @runAgda@ will fall back to
      --   vanilla Agda behaviour.
    , preCompile = foresterPreCompile
      -- ^ Called after type checking completes, but before compilation starts.
    , preModule = foresterPreModule
      -- ^ Called before compilation of each module. Gets the path to the
      --   @.agdai@ file to allow up-to-date checking of previously written
      --   compilation results. Should return @Skip m@ if compilation is not
      --   required. Will be @Nothing@ if only scope checking.
    , compileDef = foresterCompileDef
      -- ^ Compile a single definition.
    , postModule = foresterPostModule
      -- ^ Called after all definitions of a module have been compiled.
    , postCompile = foresterPostCompile
      -- ^ Called after module compilation has completed. The @IsMain@ argument
      --   is @NotMain@ if the @--no-main@ flag is present.
    , scopeCheckingSuffices = False
    , mayEraseType = const $ return False
    , backendInteractTop = Nothing
    , backendInteractHole = Nothing
    }

fFlags :: [OptDescr (Flag (ForesterOpts))]
fFlags = 
  [ Option [] ["forest"] (NoArg $ \o -> return o{optsEnabled = True}) "Generate a forest"
  , Option ['o'] ["forest-dir"] (OptArg (\r o -> case r of
       Just r -> return o{optsTreeDir = r}
       Nothing -> return o) "DIR") "directory in which tree files are paced (default: trees)"
  , Option ['S'] ["structured"] (OptArg (\r o -> case r of 
      Just "0" -> return o{optsStructured = Html}
      Just "1" -> return o{optsStructured = Semi}
      Just "2" -> return o{optsStructured = Full}
      _ -> return o) "LEVEL") "How should we proccess plain agda files?\n0 = Just emit html and a module tree linking to it\n1 = Emit module tree with highlighted source and definition trees linking to source\n2 = Fully forestify the agda code"
  ]
--   [ Option [] ["mlf"] (NoArg $ \ o -> return o{ _enabled = True })
--     "Generate Malfunction"
--   , Option ['r'] ["print-var"] (ReqArg (\r o -> return o{_resultVar = Just r}) "VAR")
--     "(DEBUG) Run the module and print the integer value of a variable"
--   , Option ['o'] [] (ReqArg (\r o -> return o{_outputFile = Just r}) "FILE")
--     "(DEBUG) Place outputFile resulting module into FILE"
--   , Option ['d'] ["debug"] (NoArg $ \ o -> return o{ _debug = True })
--     "Generate Malfunction"
--   , Option [] ["compilemlf"] (ReqArg (\r o -> return o{_outputMlf = Just r}) "MODNAME")
--     "Runs the malfunction compiler on the output file"
--   ]



foresterPreCompile :: (Monad m, MonadIO m) => ForesterOpts -> m CompEnv
foresterPreCompile (flgs)= do
    types <- liftIO (newIORef mempty)
    pure $ CompEnv flgs types
     

foresterPreModule :: (ReadTCState m)
                  => CompEnv
                  -> IsMain
                  -> TopLevelModuleName
                  -> Maybe FilePath
                  -> m (Recompile ModuleEnv ForesterModule)
foresterPreModule cenv _main tlmname _mfp = do
    -- interface <- curIF
    pure $ Recompile (ModuleEnv cenv tlmname)
-- ^ TODO: Selectively recompile only changed modules 

-- Convert the information from an agda def to a forrester definition
--   with meta data - returning the data for the resulting tree
foresterCompileDef :: CompEnv
                   -> ModuleEnv
                   -> IsMain 
                   -> Definition 
                   -> TCMT IO (Maybe ForesterDef)
foresterCompileDef cenv mn _ def = do
  let defnName = render . pretty . qnameName . defName $ def
  let fDef = ForesterDef (emptyMeta { title = (Just . pack $ defnName) , taxon = Just "agda definition"})
                          (pack $ ((render . pretty.  modEnvName $ mn) <.> defnName))
                          def
  case theDef def of
    ConstructorDefn _ -> pure Nothing
    GeneralizableVar _ -> pure Nothing
    PrimitiveDefn _ -> pure . pure $ fDef
    PrimitiveSortDefn _ -> return . pure $ fDef
    Axiom _ -> return . pure $ fDef {foresterDefTree = (foresterDefTree fDef){taxon=Just "agda postulate"}}
    FunctionDefn fd -> 
      if not . isJust $ (_funExtLam fd) then
        return . pure $ fDef {foresterDefTree = (foresterDefTree fDef){taxon=Just "agda function"}}
      else return Nothing
    -- AbstractDefn d -> return Nothing
    RecordDefn rd -> return . pure $ fDef {foresterDefTree = (foresterDefTree fDef){taxon=Just "agda record type"}}
    DatatypeDefn def -> return . pure $ fDef {foresterDefTree = (foresterDefTree fDef){taxon=Just "agda inductive definition"}}
    _ -> do
      return . pure $ fDef

-- Use the html backend to produce marked-up html
-- Construct trees for each definition - linking to the source
-- and including the relevant part of the html
-- Given the list of definitions - construct the module
-- tree - listing imports/exports, transcluding the definitions
-- and linking to the source.
foresterPostModule :: CompEnv
                    -> ModuleEnv
                    -> IsMain
                    -> TopLevelModuleName
                    -> [Maybe ForesterDef]
                    -> TCMT IO ForesterModule
foresterPostModule cenv menv main tlname defs' = do
  let defs = join . fmap (maybe [] (:[])) $ defs'
  (HtmlInputSourceFile _ ftype src hinfo) <- srcFileOfInterface tlname <$> curIF
  let defToks = splitDef $ tokenStream src hinfo 
  submods <- curIF >>= \mi -> createModTree tlname (iModuleName mi) defs (_sigSections . iSignature $ mi) defToks
  liftIO . putStrLn . render . pretty $ submods
  -- let defTrees = fmap (\fdef
  --       -> (foresterDefId fdef, definitionTree fdef (fromMaybe [] $ lookup (fmap unpack $ title $ foresterDefTree fdef) defToks)))
  --         defs
  -- mTree <- createTree tlname <$> curIF <*> pure (snd . head $ defToks) <*> pure (fmap snd defTrees)
  -- liftIO . putStrLn $ show (length defs) <> " defs in this module"

  let filePathRoot = (optsTreeDir . compileEnvOpts $ cenv )
  _ <- realiseModTree filePathRoot submods
  -- forM_ defTrees $ \(n,t) -> do
  --   liftIO $ UFT8.writeTextToFile (filePathRoot </> Ts.unpack n <.> "tree") $ T.pack . render . pretty $ t
  --   pure ()
  -- liftIO $ putStrLn $ "Inside module: " <> (render . pretty $ tlname)
  -- let filePath = filePathRoot </> (render . pretty $ tlname)
  -- liftIO $ UTF8.writeTextToFile (filePath <.> "tree") $ T.pack . render . pretty $ mTree
  -- liftIO $ UTF8.writeTextToFile (filePath <.> "html") $ renderHtml $ page False tlname (code False AgdaFileType (tokenStream src hinfo))
  liftIO . putStrLn . render . pretty {-. fmap (fmap (pretty . second (show.renderHtml))) -}.
             fmap (first show . second (const @String "...")) . splitDef $ tokenStream src hinfo 
  -- :: Bool     -- ^ Whether to generate non-code contents as-is
  --    -> FileType -- ^ Source file type
  --    -> [TokenInfo]
  --    -> Html
  pure $ ForesterModule mempty


realiseModTree :: FilePath -> Mod -> TCMT IO Tree
realiseModTree root (DataMod   mname toks defs) = return emptyTree {treeId = Just . pack.render . pretty $ mname}
realiseModTree root (RecordMod mname toks defs) = return emptyTree {treeId = Just . pack.render . pretty $ mname}
realiseModTree root (SubMods mname imps preamble smods defs) = do
  let defTrees :: [(Text, Tree)]
      defTrees = ((foresterDefId . fst) /\ uncurry definitionTree) <$> defs

  forM_ defTrees $ \(fp, t) -> do
    liftIO $ UTF8.writeTextToFile (root </> unpack fp <.> "tree") $ T.pack . render . pretty $ t

  subtrees <- mapM (realiseModTree root) smods

  let tlTree = createTree mname imps preamble (fmap snd defTrees ++ subtrees)

  liftIO $ UTF8.writeTextToFile (root </> render (pretty mname) <.> "tree") $ T.pack . render . pretty $ tlTree

  return tlTree

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

-- sectionTree :: ModuleName -> Section -> ForesterDe

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

-- definitionAnchor :: Definition -> Maybe Text
-- definitionAnchor def | defCopy def = Nothing
-- definitionAnchor def = f =<< go where
--   go :: Maybe FilePath
--   go = do
--     let name = defName def
--     case rangeModule (nameBindingSite (qnameName name)) of
--       Just f -> Just (modToFile f "html")
--       Nothing -> Nothing
--   f modn =
--     case rStart (nameBindingSite (qnameName (defName def))) of
--       Just pn -> pure $ Text.pack (modn <> "#" <> show (posPos pn))
--       Nothing -> Nothing


foresterPostCompile :: CompEnv
                    -> IsMain
                    -> Map.Map TopLevelModuleName ForesterModule
                    -> TCMT IO ()
foresterPostCompile cenv main mods = pure ()



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