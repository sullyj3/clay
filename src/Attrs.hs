{-# LANGUAGE OverloadedStrings #-}

module Attrs (myAttrMap) where

import Brick
import qualified Graphics.Vty as Vty

myAttrMap :: AttrMap
myAttrMap = attrMap Vty.defAttr [("highlight", Vty.black `on` Vty.white)]
