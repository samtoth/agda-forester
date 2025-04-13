
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Forester.Backend
  (foresterBackend) where

import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as T
import Control.DeepSeq
import GHC.Generics
import System.FilePath
import System.Process

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Compiler.Common
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Common (FileType(..))
import Agda.Interaction.Highlighting.Precise (HighlightingInfo)
import Agda.Interaction.Options (ArgDescr(..), OptDescr(..))
import Agda.Utils.Monad (join)
import Agda.Utils.Maybe (isJust)
import Agda.Utils.Impossible

import Forester.Forester
import Forester.Base
import Forester.Structured
import Forester.Html
import qualified Agda.Utils.IO.UTF8 as UTF8

import qualified Agda.Interaction.JSON as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as JSON
import Data.Foldable (toList)
import Text.Read (readMaybe)

-- import qualified Agda.Interaction.


data ForesterOpts = Opts
  { optsEnabled :: Bool
  , optsTreeDir :: FilePath
  , optsHtmlDir :: FilePath
  , optsStructured :: FStructured
  } deriving (Generic, NFData)

defaultOps :: ForesterOpts
defaultOps = Opts 
  { optsEnabled = False
  , optsTreeDir = "trees"
  , optsHtmlDir = "assets/html"
  , optsStructured = FSNone
  }

data ForesterIdent = ForesterIdent

data CompEnv = CompEnv {
    compileEnvOpts     :: ForesterOpts
  , compileForestData  :: IORef (HashMap Text Text)
  , compileTypes       :: IORef (HashMap Text ForesterIdent)
    -- ^ Hashmap from anchor→identifier for finding types while emitting
    -- HTML, and for search after
  , compileMods        :: IORef (HashMap TopLevelModuleName FileType)
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
       Just d -> return o{optsTreeDir = d}
       Nothing -> return o) "DIR") "directory in which tree files are written (default: trees)"
  , Option ['S'] ["structured"] (OptArg (\r o -> case r of 
      Just "0" -> return o{optsStructured = FSNone}
      Just "1" -> return o{optsStructured = FSSemi}
      Just "2" -> return o{optsStructured = FSFull}
      _ -> return o) "LEVEL") "How should we proccess plain agda files?\n0 = Just emit html and a module tree linking to it\n1 = Emit module tree with highlighted source and definition trees linking to source\n2 = Fully forestify the agda code"
  , Option [] ["html-dir"] (OptArg (\r o -> case r of
      Just d -> return o{optsHtmlDir = d}
      Nothing -> return o) "DIR") "director in which html files are written (default: assets/html)"
  ]

foresterPreCompile :: (Monad m, MonadIO m) => ForesterOpts -> m CompEnv
foresterPreCompile (flgs)= do
    types <- liftIO (newIORef mempty)
    mods <- liftIO (newIORef mempty)
    json <- liftIO $ readProcess "forester" ["query", "all", "proxy.toml"] []
    val <- case JSON.decodeStrictText  @JSON.Value (pack json) of
      Just (JSON.Object val) -> return val
      _ -> do
        liftIO $ putStrLn $ json
        liftIO $ putStrLn $ "For now we are just stopping - this should become a recoverable error in the future"
        error $ "Error reading forester query output"

    let metas :: JSON.Value -> [Text]
        metas (JSON.Object v) = case v JSON.!? "metas" of
          Just (JSON.Object m) -> case m JSON.!? "defines" of
            Just (JSON.String ds') -> let
                ds = case readMaybe @[String] (unpack ds') of
                        Just a -> a
                        Nothing -> error "Couldn't read String list from 'defines' meta"
              in fmap pack ds
            _ -> []
          _ -> []
        metas _ = []
    let val' :: [(Text,Text)]
        val' = join $ fmap (\(k,v) -> (,) <$> metas v <*> pure (JSON.toText k)) (JSON.toList val)
    defs <- liftIO $ newIORef (HM.fromList val')
    liftIO.putStrLn.show $ val'
    pure $ CompEnv flgs defs types mods
     

foresterPreModule :: (ReadTCState m)
                  => CompEnv
                  -> IsMain
                  -> TopLevelModuleName
                  -> Maybe FilePath
                  -> m (Recompile ModuleEnv ForesterModule)
foresterPreModule cenv _main tlmname _mfp = do
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
foresterPostModule cenv menv _main tlname defs' = do
  let defs = join . fmap (maybe [] (:[])) $ defs'
  (HtmlInputSourceFile _ ftype src hinfo) <- srcFileOfInterface tlname <$> curIF
  liftIO $ modifyIORef (compileMods cenv) (HM.insert tlname ftype)
  hm <- liftIO $ readIORef (compileMods cenv)
  ds <- liftIO $ readIORef (compileForestData cenv)
  case ftype of
    AgdaFileType -> do
      let strat = optsStructured.compileEnvOpts $ cenv
      case strat of
        FSNone -> do
          let content = renderAgda tlname (tokenStream src hinfo)
          let root = (optsHtmlDir . compileEnvOpts $ cenv )
          liftIO $ UTF8.writeTextToFile (root </> render (pretty tlname) <.> "html") $ content
        FSSemi -> __IMPOSSIBLE__ -- TODOTODOTODO!!@!!!!!!@!@!@!@!!
        FSFull -> compileModStructured cenv tlname src hinfo defs
    TreeFileType -> do
      let content = codeTree ds hm (tokenStream src hinfo)
      let root = (optsTreeDir . compileEnvOpts $ cenv )
      liftIO $ UTF8.writeTextToFile (root </> render (pretty tlname) <.> "tree") $ T.pack $ content
    _ -> __IMPOSSIBLE__
  return $ ForesterModule mempty



compileModStructured :: CompEnv -> TopLevelModuleName -> T.Text -> HighlightingInfo -> [ForesterDef] -> TCMT IO ()
compileModStructured cenv tlname src hinfo defs = do
  let defToks = splitDef $ tokenStream src hinfo 
  submods <- curIF >>= \mi -> createModTree tlname (iModuleName mi) defs (_sigSections . iSignature $ mi) defToks
  liftIO . putStrLn . render . pretty $ submods
  let filePathRoot = (optsTreeDir . compileEnvOpts $ cenv )
  hm <- liftIO $ readIORef (compileMods cenv)
  ds <- liftIO $ readIORef (compileForestData cenv)
  _ <- realiseModTree ds hm filePathRoot submods
  return ()

foresterPostCompile :: CompEnv
                    -> IsMain
                    -> Map.Map TopLevelModuleName ForesterModule
                    -> TCMT IO ()
foresterPostCompile cenv main mods = pure ()





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

