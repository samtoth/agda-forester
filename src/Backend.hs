
{-# LANGUAGE OverloadedStrings #-}
module Backend
  (foresterBackend) where

import Control.Monad.IO.Class

import Data.HashMap.Strict (HashMap)
import Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text)

import Agda.Compiler.Backend hiding (topLevelModuleName)
import Agda.Compiler.Common

import qualified Agda.Syntax.Concrete.Generic as Con
import qualified Agda.Syntax.Internal.Generic as I
import qualified Agda.Syntax.Abstract.Views as A
import qualified Agda.Syntax.Common.Aspect as Asp
import qualified Agda.Syntax.Scope.Base as Scope
import qualified Agda.Syntax.Concrete as Con
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Concrete as C


import Agda.Syntax.Translation.InternalToAbstract ( Reify(reify) )
import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_, abstractToConcreteCtx)
import Agda.Syntax.TopLevelModuleName
import Agda.Syntax.Abstract.Pretty (prettyA, prettyATop)
import Agda.Syntax.Abstract.Views
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Scope.Monad (modifyCurrentScope, getCurrentModule, freshConcreteName)
import Agda.Syntax.Abstract hiding (Type)
import Agda.Syntax.Position
import Agda.Syntax.Internal (Type, domName)
import Agda.Syntax.Fixity (Precedence(TopCtx))
import Agda.Syntax.Common
import Agda.Syntax.Info

import qualified Agda.TypeChecking.Monad.Base as I
import qualified Agda.TypeChecking.Pretty as P
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Records (isRecord)
import Agda.TypeChecking.Reduce (instantiateFull, reduceDefCopyTCM, normalise)
import Agda.TypeChecking.Level (reallyUnLevelView)

import qualified Agda.Utils.Maybe.Strict as S
import Agda.Utils.FileName
import Agda.Utils.Lens
import Agda.Utils.Size

import Agda.Interaction.Options (ArgDescr(..), OptDescr(..), Flag)


type ForesterOpts = ()

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
    
type Flags = ()

data ForesterTreeMeta = ForesterTreeMeta

data ForesterDef = ForesterDef 
    { foresterDefTree :: ForesterTreeMeta

    }

foresterBackend :: FilePath -> Backend
foresterBackend = Backend  . foresterBackend'

foresterBackend' :: FilePath -> Backend' (FilePath, ForesterOpts) CompEnv ModuleEnv ForesterModule ForesterDef
foresterBackend' basePath = Backend' 
    { backendName = "forester"
      -- ^ the name of the backend
    , backendVersion = Nothing
      -- ^ Optional version information to be printed with @--version@.
    , options = (basePath, ())
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

fFlags :: [OptDescr (Flag (FilePath, ForesterOpts))]
fFlags = []
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



foresterPreCompile :: (Monad m, MonadIO m) => (FilePath, Flags) -> m CompEnv
foresterPreCompile (_, flgs)= do
    types <- liftIO (newIORef mempty)
    pure $ CompEnv flgs types
     

foresterPreModule :: (ReadTCState m)
                  => CompEnv
                  -> IsMain
                  -> TopLevelModuleName
                  -> Maybe FilePath
                  -> m (Recompile ModuleEnv ForesterModule)
foresterPreModule cenv _main tlmname _mfp = do
    interface <- curIF
    pure $ Recompile (ModuleEnv cenv tlmname)
-- ^ TODO: Selectively recompile only changed modules 

-- Convert the information from an agda def to a forrester definition
--   with meta data - returning the data for the resulting tree
foresterCompileDef :: CompEnv
                   -> ModuleEnv
                   -> IsMain 
                   -> Definition 
                   -> TCMT IO ForesterDef
foresterCompileDef cenv mn def = _

-- Given a list of definitions - construct the module
-- page transcluding the definitions
foresterPostModule :: CompEnv
                    -> ModuleEnv
                    -> IsMain
                    -> TopLevelModuleName
                    -> [ForesterDef]
                    -> TCMT IO ForesterModule
foresterPostModule = _


foresterPostCompile :: CompEnv
                    -> IsMain
                    -> Map TopLevelModuleName ForesterModule
                    -> TCMT IO ()
foresterPostCompile = _