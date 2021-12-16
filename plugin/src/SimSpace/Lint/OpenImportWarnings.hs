{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: A plugin that detects open imports
-- (equivalent to @-Wwarn-missing-import-lists@) but doesn't warn on the ones
-- that we don't care about:
-- * preludes
-- * module under test
module SimSpace.Lint.OpenImportWarnings (plugin) where

import Bag
import Data.Foldable (all, traverse_)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Debug.Trace
import ErrUtils
import GHC.Hs
import GHC.Hs.Extension
import GhcPlugins
import Prelude hiding ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = parsed,
      pluginRecompile = purePlugin
    }

parsed ::
  [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsed _options _summary mod = do
  let imports = getImports mod
  flags <- getDynFlags
  traverse_ (emitWarnings . warningsForImport flags (modName mod)) imports
  pure mod

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
      Just (True, _) -> error "impossible"
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
