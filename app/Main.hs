{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Vector as Vec

import System.FilePath (takeFileName)
import System.Directory
import System.IO
import Control.Monad.IO.Class

import Lens.Micro.TH
import Lens.Micro

-- Lens cheatsheet:
-- (^.) view (upside down ^ for view)
-- (%~) over (% = mod = modify)
-- (.~) set (sideways ~ for set)
--
import Brick
import qualified Brick.Widgets.Edit as Ed
import qualified Brick.Widgets.List as BL (handleListEvent, listSelectedElement)
-- import qualified Brick.Focus as Foc
import qualified Graphics.Vty as V

import qualified FilterEditor as FE
import qualified FileList as FL
import Attrs

------------------------
data ResName = Header | Filter | FileList
  deriving (Eq, Ord, Show)

data AppState = AppState
    { _cwdState :: CWDState
    , _showHidden :: Bool

    , _filterEditor :: FE.FilterEditor ResName
    , _fileList :: FL.FileList ResName
    }

data CWDState = CWDState
    { _cwd :: FilePath
    , _filesCWD :: Vec.Vector FilePath
    }

-- use templatehaskell to generate lenses
makeLenses ''AppState
makeLenses ''CWDState

-- header widget containing the current working directory
currPathHeader :: FilePath -> Widget ResName
currPathHeader = withAttr "highlight" . padRight Max . str

draw :: AppState -> [Widget ResName]
draw state = [root]
  where
    root :: Widget ResName
    root = vBox [ currPathHeader             (state ^. (cwdState . cwd))
                , FE.renderFilterEditor      (state ^. filterEditor) True
                , FL.renderFileList     True (state ^. fileList)
                ]

isFilterKey :: V.Key -> Bool
isFilterKey (V.KChar _c)  = True
isFilterKey (V.KBS)       = True
isFilterKey _k            = False

isListKey :: V.Key -> Bool
isListKey (V.KUp)   = True
isListKey (V.KDown) = True
isListKey _k        = False

eventHandler :: AppState -> BrickEvent ResName () -> EventM ResName (Next AppState)
eventHandler state (VtyEvent ev) =
  do
    () <- liftIO $ hPutStrLn stderr $ "handling " ++ show ev
    case ev of
      (V.EvKey V.KEsc _)                -> halt        state
      (V.EvKey V.KLeft _)               -> handleLeft  state
      (V.EvKey V.KRight _)              -> handleRight state
      (V.EvKey (V.KChar '.') [V.MMeta]) -> continue $ toggleShowHidden state
      (V.EvKey k _)
         | isFilterKey k -> continue =<< handleEventLensed state
                                                           filterEditor
                                                           Ed.handleEditorEvent
                                                           ev
         | isListKey   k -> continue =<< handleEventLensed state
                                                           fileList
                                                           BL.handleListEvent
                                                           ev
      _ -> do
        () <- liftIO $ hPutStrLn stderr $ "unhandled VtyEvent: " ++ show ev
        halt state
eventHandler state _event = continue state

handleLeft :: AppState -> EventM ResName (Next AppState)
handleLeft state = continue =<< liftIO (goUp state)

-- if selected path is dir, cd to it
handleRight :: AppState -> EventM ResName (Next AppState)
handleRight state = do
  let selectedPath = state ^. fileList & BL.listSelectedElement
  case selectedPath of
    Nothing -> continue state
    Just (_ix, fp) -> do
      isDir <- liftIO $ doesDirectoryExist fp
      if isDir
        then continue =<< liftIO (cd fp state)
        else continue state

toggleShowHidden :: AppState -> AppState
toggleShowHidden s =
  let showHidden' = not $ s ^. showHidden
      fileList'   = (FL.updateFileList showHidden' (s ^. cwdState . filesCWD) (s ^. fileList))
  in
    s { _showHidden = showHidden', _fileList = fileList' }

chooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor _state _locations = Nothing

myApp :: App AppState () ResName
myApp = App { appDraw = draw
            , appChooseCursor = chooseCursor
            , appHandleEvent = eventHandler
            , appStartEvent = return
            , appAttrMap = const myAttrMap
            }


---------------

cd :: FilePath -> AppState -> IO AppState
cd p s = do
  setCurrentDirectory p
  cwdState' <- getCWDState
  let fileList' = FL.updateFileList
                    (s ^. showHidden)
                    (cwdState' ^. filesCWD)
                    (s ^. fileList)

  return s { _cwdState = cwdState', _fileList = fileList' }

-- once the directory has moved up a level, selects the child path we were just in
-- in the fileList
goUp :: AppState -> IO AppState
goUp s = do
  let prevPath = takeFileName $ s ^. (cwdState . cwd)
  s' <- cd ".." s

  return $ s' & fileList %~ (FL.selectPath prevPath)

getCWDState :: IO CWDState
getCWDState = do
  cwd' <- getCurrentDirectory
  dirContents <- Vec.fromList <$> listDirectory cwd'

  return CWDState { _cwd=cwd'
                  , _filesCWD=dirContents}

makeInitialState :: IO AppState
makeInitialState = do
  initialCWDState <- getCWDState

  let emptyFileList = FL.makeFileList FileList
  let showHidden_ = False
  let fileList_ = FL.updateFileList showHidden_ (initialCWDState ^. filesCWD) emptyFileList

  return $ AppState { _cwdState = initialCWDState
                    , _showHidden = showHidden_

                    , _filterEditor = FE.makeFilterEditor Filter
                    , _fileList = fileList_
                    }

main :: IO AppState
main = do
  initialState <- makeInitialState
  defaultMain myApp initialState
