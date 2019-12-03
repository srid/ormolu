{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC
import Ormolu.Imports
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma
import qualified Data.Set as E
import Data.Maybe (mapMaybe)

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Ormolu.Parser.CommentStream

-- | Render a module.
p_hsModule ::
  -- | Shebangs
  [Located String] ->
  -- | Pragmas
  [Pragma] ->
  -- | AST to print
  ParsedSource ->
  R ()
p_hsModule shebangs pragmas (L moduleSpan HsModule {..}) = do
  -- If span of exports in multiline, the whole thing is multiline. This is
  -- especially important because span of module itself always seems to have
  -- length zero, so it's not reliable for layout selection.
  let exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
      deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      spans' = exportSpans ++ deprecSpan ++ [moduleSpan]
  switchLayout spans' $ do
    forM_ shebangs $ \x ->
      located x $ \shebang -> do
        txt (T.pack shebang)
        newline
    spitStackHeader
    newline
    p_pragmas pragmas
    newline
    case hsmodName of
      Nothing -> return ()
      Just hsmodName' -> do
        located hsmodName' $ \name -> do
          forM_ hsmodHaddockModHeader (p_hsDocString Pipe True)
          p_hsmodName name
        forM_ hsmodDeprecMessage $ \w -> do
          breakpoint
          located' p_moduleWarning w
        case hsmodExports of
          Nothing -> return ()
          Just hsmodExports' -> do
            breakpoint
            inci (p_hsmodExports (unLoc hsmodExports'))
        breakpoint
        txt "where"
        newline
    newline
    let sortedImports = sortImports hsmodImports
        commentedLines = E.fromList (mapMaybe importLine sortedImports)
    case listToMaybe sortedImports of
      Nothing -> return ()
      Just (x : _) -> do
        let firstLine = fromJust (lookupMin commentedLines)
            lastLine = fromJust (lookupMax commentedLines) + 1
            importSectionSpn =
              mkSrcSpan (mkSrcLoc "" firstLine 0) (mkSrcLoc "" lastLine 0)
        -- This should output (and remove from the comment stream) all
        -- comments that go before the import section.
        located (L importSectionSpn ()) $ \() -> do
          -- We want to sort imports before printing them, which means that
          -- outputting corresponding comments is trickier and we cannot just use
          -- our familiar 'located' helpers. 'located' picks up and outputs
          -- anything up to current position, so that if the last import becomes
          -- the first after sorting all comments will be dumped before it.
          --
          -- Instead we pop all comments that are within the import section before
          -- we start outputting comments and then we manually match and output
          -- them as we go through the import list.

          -- TODO pop comments form the import region and assign them to different
          -- imports as designated by their starting lines in the original input.

          let importComments :: Map Int (NonEmpty Comment)
              importComments = undefined

          forM_ sortedImports $ \x ->
            -- TODO for every import printing action temporarily replace comment
            -- stream by using a new primitive combinator for that. Then use
            -- located as usual for actual outputting of the comment.
            located' p_hsmodImport x

    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
