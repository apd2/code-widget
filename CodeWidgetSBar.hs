module CodeWidgetSBar where

import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Text.Parsec
import Data.IORef
import Util
import CodeWidgetTypes
import CodeWidgetUtil
import CodeWidgetInternal


createSBar :: IO CwSBar
createSBar = do
    sbe  <- G.entryNew
    G.entrySetHasFrame sbe True
    G.entrySetWidthChars sbe 8
    G.entrySetAlignment sbe 0.5
    G.set sbe [G.entryEditable G.:= False]
    return sbe


csbSwitchPage :: RCodeView -> Int -> IO ()
csbSwitchPage ref p = do
  cv <- readIORef ref
  csbCursUpdate cv p

csbCursMove :: RCodeView -> G.MovementStep -> Int -> Bool -> IO ()
csbCursMove ref _ _ _ = do
  cv  <- readIORef ref
  nbi <- cvCurPage cv
  csbCursUpdate cv nbi



csbCursUpdate :: CodeView -> Int -> IO ()
csbCursUpdate cv p = do 
  case getContexts cv (Region p rootRegion) of
        Nothing      -> return ()
        Just (pg, _) -> do pos <- cvCursorPos pg
                           let tpos = show (sourceLine pos) ++ "," ++ show (sourceColumn pos)
                           G.entrySetText (cvSBar cv) tpos


