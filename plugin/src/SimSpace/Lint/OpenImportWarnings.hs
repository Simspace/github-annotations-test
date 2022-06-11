{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: A plugin that detects open imports
-- (equivalent to @-Wmissing-import-lists@) but doesn't warn on the ones
-- that we don't care about:
-- * preludes
-- * module under test
--
-- Enable this plugin by using @-fplugin=SimSpace.Lint.OpenImportWarnings@.
--
-- Disable this plugin locally by using @-fplugin-opt=SimSpace.Lint.OpenImportWarnings:disable@.
module SimSpace.Lint.OpenImportWarnings (plugin) where

import Bag (emptyBag, unionBags, unionManyBags, unitBag)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import ErrUtils (WarningMessages, mkPlainWarnMsg)
import GHC.Hs
  ( IE(IEDoc, IEDocNamed, IEGroup, IEModuleContents, IEThingAbs, IEThingAll, IEThingWith, IEVar, XIE)
  , ImportDecl(ideclHiding, ideclName, ideclQualified), GhcPs, LIE, LImportDecl, hsmodImports
  , hsmodName, isImportDeclQualified, noExtCon
  )
import GhcPlugins
  ( GenLocated(L), Hsc(Hsc), Plugin(parsedResultAction, pluginRecompile), (<>), CommandLineOption
  , DynFlags, HsParsedModule, ModSummary, ModuleName, defaultPlugin, getDynFlags, hpm_module
  , moduleNameString, ppr, purePlugin, unLoc
  )
import Prelude hiding ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T

plugin :: Plugin
plugin =
  id $ defaultPlugin
    { parsedResultAction = parsed,
      pluginRecompile = purePlugin
    }

parsed ::
  [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsed options _summary m = do
  when (not ("disable" `elem` options)) $ do
    let imports = getImports m
    flags <- getDynFlags
    traverse_ (emitWarnings . warningsForImport flags (modName m)) imports
  pure m

modName :: HsParsedModule -> Maybe ModuleName
modName = fmap unLoc . hsmodName . unLoc . hpm_module

importName :: ImportDecl GhcPs -> ModuleName
importName = unLoc . ideclName

getImports :: HsParsedModule -> [LImportDecl GhcPs]
getImports = hsmodImports . unLoc . hpm_module

alwaysAllowedOpenImports :: Set Text
alwaysAllowedOpenImports =
  Set.fromList
    [ "SimSpace.Prelude",
      "Prelude",
      "SimSpace.Lens"
    ]

moduleNameText :: ModuleName -> Text
moduleNameText = T.pack . moduleNameString

-- | Like or, but require at least one result to be True
oneOf :: [Bool] -> Bool
oneOf [] = False
oneOf xs = or xs

isAllowedOpenImport ::
  -- | Name of the module containing the import if it exists
  Maybe ModuleName ->
  -- | Name of the module being imported
  ModuleName ->
  Bool
isAllowedOpenImport (fmap moduleNameText -> mp) (moduleNameText -> i) =
  i `Set.member` alwaysAllowedOpenImports || isTestImport
  where
    -- We allow test imports to be open imports; we detect this by approximating
    -- via stripping Spec/Test/Tests from the end of the name of the current
    -- module and checking if that matches the name of the imported module.
    isTestImport :: Bool
    isTestImport = fromMaybe True . flip fmap mp $ \p ->
      let strippedParentNames = T.stripSuffix <$> ["Spec", "Test", "Tests"] <*> pure p
       in oneOf . mapMaybe (fmap (== i)) $ strippedParentNames

isOpenImport :: ImportDecl GhcPs -> Bool
isOpenImport i
  | not (isImportDeclQualified (ideclQualified i)) = case ideclHiding i of
    Just (False, _) -> False
    _ -> True
  | otherwise = False

warningsForImport ::
  DynFlags -> Maybe ModuleName -> LImportDecl GhcPs -> WarningMessages
warningsForImport flags mp (L l i)
  | maybe False (("Paths_" `T.isPrefixOf`) . moduleNameText) mp = emptyBag
  | isOpenImport i && not (isAllowedOpenImport mp (importName i)) =
    unitBag . mkPlainWarnMsg flags l $
      "The module ‘" <> ppr (importName i) <> "’ does not have an explicit import list"
  | otherwise = case ideclHiding i of
      Just (False, L _ items) -> unionManyBags (map (warningsForImportItem flags) items)
      Just (True, _) -> emptyBag
      Nothing -> emptyBag

warningsForImportItem :: DynFlags -> LIE GhcPs -> WarningMessages
warningsForImportItem flags (L l ie) = case ie of
  ii@IEThingAll {} ->
    unitBag . mkPlainWarnMsg flags l $
      "The import item ‘" <> ppr ii <> "’ does not have an explicit import list"
  IEVar {} -> emptyBag
  IEThingAbs {} -> emptyBag
  IEThingWith {} -> emptyBag
  IEModuleContents {} -> emptyBag
  IEGroup {} -> emptyBag
  IEDoc {} -> emptyBag
  IEDocNamed {} -> emptyBag
  XIE nec -> noExtCon nec

emitWarnings :: WarningMessages -> Hsc ()
emitWarnings newMessages = Hsc $ \_env messages ->
  pure ((), messages `unionBags` newMessages)
