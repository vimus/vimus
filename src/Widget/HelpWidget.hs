{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Widget.HelpWidget (
  makeHelpWidget

-- exported to silence warnings
, CommandList (..)
, HelpWidget (..)
) where

import           Data.List (intercalate)
import           Control.Applicative
import           Text.Printf (printf)
import           Data.String
import           Data.Map (Map)
import qualified Data.Map as Map

import           Vimus
import           Command.Core (Command, commandName, commandDescription, commandSynopsis)
import           Widget.ListWidget (ListWidget)
import qualified Widget.ListWidget as ListWidget
import           Widget.TextWidget
import           Type
import           Content
import           WindowLayout

data HelpWidget = HelpWidget {
  helpWidgetCommandList  :: CommandList
, helpWidgetDetailedHelp :: Maybe AnyWidget
}

makeHelpWidget :: [Command] -> Map String [String] -> AnyWidget
makeHelpWidget commands macroGuesses = AnyWidget (HelpWidget commandList Nothing)
  where
    commandList = CommandList (ListWidget.new commands) macroGuesses

-- helper for searchItem and filterItem pass-through
passThrough :: (forall a . Widget a => (a -> a)) -> HelpWidget -> HelpWidget
passThrough f (HelpWidget commandList mDetails) = case mDetails of
  Just details -> HelpWidget commandList (Just $ f details)
  Nothing      -> HelpWidget (f commandList) Nothing

commandHelp :: Command -> [TextLine]
commandHelp c = TextLine [Colored SuggestionsColor $ commandSynopsis c] : (map (fromString . ("  " ++)) .  lines . commandDescription) c

instance Widget HelpWidget where
  render (HelpWidget commandList mDetails) = maybe (render commandList) render mDetails
  currentItem _                            = Nothing
  searchItem widget o t                    = passThrough (\w -> searchItem w o t) widget
  filterItem widget t                      = passThrough (`filterItem` t) widget

  handleEvent widget@(HelpWidget commandList mDetails) ev  = case ev of

    -- switch between command list and details on :default-action
    EvDefaultAction -> maybe moveIn (const moveOut) mDetails

    -- show details on :move-in
    EvMoveIn        -> moveIn

    -- go back to command list on :move-out
    EvMoveOut       -> moveOut

    -- pass through all other events
    _               -> passThrough_
    where
      passThrough_ = case mDetails of
        Just details -> HelpWidget commandList . Just <$> handleEvent details ev
        Nothing      -> (`HelpWidget` Nothing) <$> handleEvent commandList ev

      moveOut = return $ HelpWidget commandList Nothing

      moveIn = return $ case (mDetails, selectCommand commandList) of

        -- command selected, show details
        (Nothing, Just c) -> HelpWidget commandList (Just . makeTextWidget $ commandHelp c)

        -- already showing details (or no command under cursor), do nothing
        _ -> widget

data CommandList = CommandList {
  commandListCommands     :: ListWidget Command
, commandListMacroGuesses :: Map String [String]
}

instance Searchable Command where
  searchTags = return . commandName

-- NOTE: This is not really used, but we need to satisfy the type checker.
instance Renderable Command where
  renderItem = commandName

selectCommand :: CommandList -> Maybe Command
selectCommand = ListWidget.select . commandListCommands

instance Widget CommandList where
  render (CommandList w ms) = do
    render (fmap help w)
    where
      help c = printf "%-30s" (commandSynopsis c) ++ macros
        where
          -- macros defined for this command
          macros = maybe "" (intercalate "  " . map formatMacro) mMacros
          mMacros = Map.lookup (commandName c) ms

          formatMacro :: String -> String
          formatMacro = printf "%-10s"

  currentItem _                      = Nothing
  searchItem  (CommandList w ms) o t = CommandList (searchItem w o t) ms
  filterItem  (CommandList w ms) t   = CommandList (filterItem w t) ms
  handleEvent (CommandList w ms) ev  = (`CommandList` ms) <$> handleEvent w ev
