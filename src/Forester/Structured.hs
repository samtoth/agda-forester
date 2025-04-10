{-# LANGUAGE OverloadedStrings #-}
module Forester.Structured where

import Agda.Syntax.Common.Pretty
import Agda.Compiler.Backend

import Forester.Base


data Mod
  = SubMods ModuleName [TopLevelModuleName] [TokenInfo] [Mod] [(ForesterDef, [TokenInfo])]
  | DataMod ModuleName [TokenInfo] [ForesterDef]
  | RecordMod ModuleName [TokenInfo] [ForesterDef]

instance Pretty Mod where
  pretty (SubMods mn _ _ smods defs) 
    = pretty mn <> ": " <> vsep [pretty smods, pretty (foresterDefId . fst <$> defs)]
  pretty (DataMod nm _ defs) = "Data" <> parens (pretty nm) <> ": " <> pretty (foresterDefId <$> defs)
  pretty (RecordMod nm _ defs) = "Record" <> parens (pretty nm) <> ": " <> pretty (foresterDefId <$> defs)
