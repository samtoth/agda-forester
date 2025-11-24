
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications, LambdaCase #-}
module Forester.Backend
  (foresterBackend) where

import Control.Monad.IO.Class
import Control.Monad.Catch (catch)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Lazy as BS hiding (pack)

import Control.DeepSeq
import GHC.Generics
import System.FilePath
import System.Process
import System.Directory
import Unsafe.Coerce

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Compiler.Common
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Common (FileType(..))
import Agda.Interaction.Highlighting.Precise (HighlightingInfo)
import Agda.Interaction.Options (ArgDescr(..), OptDescr(..))
import Agda.Utils.Monad (join)
import Agda.Utils.Maybe (isJust)
import Agda.Utils.FileName
import Agda.Utils.Impossible
import Agda.TypeChecking.Serialise

import Forester.Forester
import Forester.Base
import Forester.Html
import Forester.Data
import qualified Agda.Utils.IO.UTF8 as UTF8

import qualified Agda.Interaction.JSON as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as JSON
import Data.Foldable (toList)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-- import qualified Agda.Interaction.


data ForesterOpts = Opts
  { optsEnabled :: Bool
  , optsTreeDir :: FilePath
  , optsHtmlDir :: FilePath
  -- , optsStructured :: FStructured
  } deriving (Generic, NFData)

defaultOps :: ForesterOpts
defaultOps = Opts
  { optsEnabled = False
  , optsTreeDir = "trees"
  , optsHtmlDir = "assets/html"
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


foresterBackend :: Backend
foresterBackend = Backend foresterBackend'

foresterBackend' ::  Backend' ForesterOpts CompEnv ModuleEnv ForesterModule ()
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
  , Option [] ["fhtml-dir"] (OptArg (\r o -> case r of
      Just d -> return o{optsHtmlDir = d}
      Nothing -> return o) "DIR") "directory in which forester HTML files are written (default: assets/html)"
  ]

foresterPreCompile :: ForesterOpts -> TCMT IO CompEnv
foresterPreCompile flgs = do
    types <- liftIO $ newIORef mempty -- types' 

    mods' <- liftIO (doesFileExist "forest-map.json") >>= \case
      False -> pure mempty
      True -> do
        mapRaw <- liftIO $ BS.readFile "forest-map.json"
        case JSON.eitherDecode mapRaw of
          Right a -> pure a
          Left err -> liftIO (putStrLn $ "Warning: Not able to read forest-map.json: " <> err) >> pure mempty
    mods <- liftIO $ newIORef mods'
    -- liftIO.putStrLn.show $ val' <ft <- iFileType <$> curIF
    pure $ CompEnv flgs types mods

foresterPreModule :: (ReadTCState m, MonadIO m)
                  => CompEnv
                  -> IsMain
                  -> TopLevelModuleName
                  -> Maybe FilePath
                  -> m (Recompile ModuleEnv ForesterModule)
foresterPreModule cenv _main tlmname mifp = do
    let
      root = (optsTreeDir . compileEnvOpts $ cenv )
      path = root </> render (pretty tlmname) <.> "tree"
    liftIO $ do
      uptd <- uptodate path
      if uptd
        then do
          putStrLn $ "tree for module " <> render (pretty tlmname) <> " is up-to-date"
          pure $ Skip ForesterModule
        else pure $ Recompile (ModuleEnv cenv tlmname)
      where
        uptodate a = case mifp of
                       Nothing    -> pure False
                       Just ifile -> isNewerThan a ifile

-- Convert the information from an agda def to a forester definition
--   with meta data - returning the data for the resulting tree
foresterCompileDef :: CompEnv
                   -> ModuleEnv
                   -> IsMain
                   -> Definition
                   -> TCMT IO ()
foresterCompileDef cenv mn _ def = do
  let defnName = render . pretty . qnameName . defName $ def
  let dType = defType def

  -- Modify the forestInfoHashMap - if nothing is
  -- in the HM, add an entry with the type,
  -- module name as the treeId and qname.
  -- If it is already in there, just update it's
  -- type - as the treeId may contain more refined information
  liftIO $ modifyIORef (compileForestData cenv) $
    HM.alter
      (\case
          Just f -> Just $ f {fty = dType}
          Nothing -> Just (FInfo {fqname = defName def, ftId = Nothing, fty = dType}))
      (pack defnName)

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
                    -> [()]
                    -> TCMT IO ForesterModule
foresterPostModule cenv menv _main tlname defs' = do
  (HtmlInputSourceFile _ ftype src hinfo) <- srcFileOfInterface tlname <$> curIF
  -- TODO: run treelist to get a tree map
  case ftype of
    AgdaFileType -> do
      liftIO $ modifyIORef (compileMods cenv) (HM.insert (pack.render.pretty$tlname) (ftype, []))
      hm <- liftIO $ readIORef (compileMods cenv)
      ds <- liftIO $ readIORef (compileForestData cenv)
      let content = renderAgda tlname (tokenStream src hinfo) -- TODO: Links are broken for this - they always point ...html
      let root = (optsHtmlDir . compileEnvOpts $ cenv )
      liftIO $ UTF8.writeTextToFile (root </> render (pretty tlname) <.> "html") $ content
      liftIO $ putStrLn $ "Written " <> render (pretty tlname) <> " to " <> (root </> render (pretty tlname) <.> "html")
    TreeFileType -> do
      treelistOutput <- liftIO $ readProcess "treelist" [T.unpack src] []
      let res = JSON.eitherDecode (T.encodeUtf8 . T.pack $ treelistOutput)
      cs <- case res of 
        Right a -> pure a
        Left err -> error $ "Unexpected failure to read treelist output (" <> err <> "):\n" <> treelistOutput
      liftIO $ modifyIORef (compileMods cenv) (HM.insert (pack.render.pretty$tlname) (ftype, cs))
      hm <- liftIO $ readIORef (compileMods cenv)
      ds <- liftIO $ readIORef (compileForestData cenv)
      let content = codeTree ds hm (tokenStream src hinfo)
      let root = (optsTreeDir . compileEnvOpts $ cenv )
      liftIO $ UTF8.writeTextToFile (root </> render (pretty tlname) <.> "tree") $ T.pack $ content
      liftIO $ putStrLn $ "Written " <> render (pretty tlname) <> " to " <> (root </> render (pretty tlname) <.> "tree")
    _ -> __IMPOSSIBLE__
  return $ ForesterModule

-- Post compile we will write the forester defs to disk to use next time
foresterPostCompile :: CompEnv
                    -> IsMain
                    -> Map.Map TopLevelModuleName ForesterModule
                    -> TCMT IO ()
foresterPostCompile cenv main mods = do
  types <- liftIO $ readIORef (compileMods cenv)
  liftIO $ JSON.encodeFile "forest-map.json" types
