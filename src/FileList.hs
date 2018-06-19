{-# LANGUAGE OverloadedStrings #-}

module FileList ( FileList
                , makeFileList
                , renderFileList
                , selectPath
                , selectPathUnsafe
                , updateFileList ) where

import Data.Maybe (fromJust)
import qualified Data.Vector as Vec

import Brick
import qualified Brick.Widgets.List as BL

type FileList n = BL.List n FilePath

makeFileList :: n -> FileList n
makeFileList name = BL.list name (Vec.fromList []) 1

isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _        = False

withoutHidden :: Vec.Vector FilePath -> Vec.Vector FilePath
withoutHidden = Vec.filter (not . isHidden)

updateFileList :: Bool -> Vec.Vector FilePath -> FileList n -> FileList n
updateFileList showHidden paths fl =
  let
    paths' = if showHidden then paths
                           else withoutHidden paths
  in
    BL.listReplace paths' (Just 0) fl


-- do nothing if path does not exist. This could happen if we're in directory d, and d is
-- deleted in another program. Then when we go up from d, we would try to select d in our list
-- but it wouldn't exist. We should do nothing instead of crashing in that case
selectPath :: FilePath -> FileList n -> FileList n
selectPath path fl = case mPathIdx of
                       (Just pathIdx) -> BL.listMoveTo pathIdx fl
                       Nothing        -> fl
  where
    mPathIdx = Vec.findIndex (== path) (BL.listElements fl)

selectPathUnsafe :: FilePath -> FileList n -> FileList n
selectPathUnsafe path fl = BL.listMoveTo (fromJust mPathIdx) fl 
  where
    mPathIdx = Vec.findIndex (== path) (BL.listElements fl)


renderPath :: Bool -- is the path selected?
           -> FilePath
           -> Widget n
renderPath True  path = withAttr "highlight" . padRight Max . str $ path
renderPath False path = padRight Max . str $ path

renderFileList :: (Ord n, Show n) => Bool -> FileList n -> Widget n
renderFileList focussed fileList = BL.renderList renderPath focussed fileList
