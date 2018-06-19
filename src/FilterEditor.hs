{-# LANGUAGE OverloadedStrings #-}

module FilterEditor (FilterEditor, makeFilterEditor, renderFilterEditor) where

import Brick
import qualified Brick.Widgets.Edit as Ed
import qualified Data.Text as T

type FilterEditor n = Ed.Editor T.Text n

makeFilterEditor :: (Ord n, Show n) => n -> FilterEditor n
makeFilterEditor name = Ed.editorText name (Just 1) ""

renderFilterEditor :: (Ord n, Show n) => Ed.Editor T.Text n -> Bool -> Widget n
renderFilterEditor e hasFocus = Ed.renderEditor (txt . T.unwords)
                                                hasFocus
                                                e

