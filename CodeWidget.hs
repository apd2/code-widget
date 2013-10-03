module CodeWidget (codeWidgetNew,
                   Code(..), 
                   CwAPI(..),
                   CwSelection(..),
                   Region, 
                   RegionID,
                   PageID,
                   rootRegion, 
                   CwView, 
                   CwIter ) where

import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Data.IORef
import CodeWidgetTypes
import CodeWidgetAPI
import CodeWidgetSBar

{--
design considerations:

1. The way the user interacts with the debugger is going to change many times as we 
experiment with it and, later, increase the level of automation.  Ideally, the 
source view widget interface should be general enough to accommodate these changes.  
Hence I believe that the interface should be defined in terms of nested text regions, 
rather than magic blocks and other synthesis-specific concepts.

2. Functions exported by the widget will take text coordinates.  These coordinates should be 
relative to the current region and should not be sensitive to changes in nested regions, e.g.:

BEGIN_STATIC_REGION
text
text
{nested_editable_region}
highlighted_text
END_STATIC_REGION

In this example, when the user adds some text to the editable region, coordinates of the highlighted region are returned as if the editable region was empty, i.e., had zero width and height.
--}


-- API creation

{-- codeWidgetNew - create an instance of the code widget.
              codeWidgetNew :: String       -- language type (file extension)
                            -> Int       -- width
                            -> Int       -- height
                            -> IO Code
--}
codeWidgetNew :: String -> Int -> Int -> IO Code
codeWidgetNew l w h = do
    slm <- G.sourceLanguageManagerGetDefault
    mlng <- G.sourceLanguageManagerGetLanguage slm l
    lng <- (case mlng of
                  Nothing -> error $ "Can't find " ++ l ++ " Language Definition"
                  Just x  -> return x)
    nb <- G.notebookNew
    font <- G.fontDescriptionFromString fontSrc
    G.widgetSetSizeRequest nb w h
    G.widgetShow nb
    sbar <- createSBar 
    G.widgetShow $ G.toWidget sbar

    ref <- newIORef $ CodeView { cvNotebook   = nb
                               , cvSBar       = sbar
                               , cvLanguage   = lng
                               , cvPages      = []
                               , cvFont       = font
                               , cvNextPage   = 0
                               }

    _ <- G.after nb G.switchPage (csbSwitchPage ref)

    return $ Code { codeApi = CwAPI { pageCreate            = codePageCreate       ref
                                    , regionCreate          = codeRegionCreate     ref
                                    , regionCreateFrom      = codeRegionCreateFrom ref
                                    , regionDelete          = codeRegionDelete     ref
                                    , regionGetText         = codeRegionGetText    ref
                                    , regionGetBoundedText  = codeRegionGetBoundedText   ref
                                    , regionSetText         = codeRegionSetText    ref
                                    , regionDeleteText      = codeRegionDeleteText ref
                                    , regionInsertText      = codeRegionInsertText ref
                                    , regionGetAllText      = codeGetAllText       ref
                                    , tagNew                = codeTagNew           ref
                                    , regionApplyTag        = codeRegionApplyTag   ref
                                    , regionRemoveTag       = codeRegionRemoveTag  ref
                                    , regionSetMark         = codeRegionSetMark    ref
                                    , regionGetIter         = codeRegionGetIter    ref
                                    , regionGetSelection    = codeRegionGetSelection ref
                                    , regionScrollToPos     = codeRegionScrollToPos ref
                                    , regionEditable        = codeRegionEditable   ref
                                    , dumpRegions           = codeDumpRegions      ref
                                    }
                  , codeWidget = G.toWidget nb
                  , codePos    = G.toWidget sbar
                  }



