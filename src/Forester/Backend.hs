
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Forester.Backend
  (foresterBackend) where

import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text, pack, unpack)
import Control.DeepSeq
import GHC.Generics
import System.FilePath
import Data.Bifunctor (first, second)

import Agda.Compiler.Backend hiding (topLevelModuleName, Name, Constructor)
import Agda.Compiler.Common
import Agda.Syntax.Common.Pretty
import Agda.Interaction.Options (ArgDescr(..), OptDescr(..))
import Agda.Utils.Monad (join)
import Agda.Utils.Maybe (isJust)

import Forester.Forester
import Forester.Base
import Forester.Structured
import Forester.Html


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




