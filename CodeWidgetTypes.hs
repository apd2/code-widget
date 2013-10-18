module CodeWidgetTypes (Code(..),
                        CwAPI(..),
                        CwSelection(..),
                        Region(..),
                        PageID,
                        fontSrc,
                        CwView,
                        CwIter,
                        CwRef,
                        CwSBar,
                        RegionID,
                        rootRegion,
                        noRegion,
                        RegionContext(..),
                        PageContext(..),
                        CodeView(..),
                        RCodeView) where

import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Text.Parsec
import Data.IORef
import Util


--------------------------------------------------------------
-- Constants
--------------------------------------------------------------
fontSrc    = "Courier Regular 9"
--fontSrc    = "Monospace Regular 9"
--fontSrc    = "FreeMono Medium 9"


--------------------------------------------------------------
-- Types
--------------------------------------------------------------
--encapsulate types from gtk
type CwView  = G.SourceView
type CwIter  = G.TextIter
type CwSBar  = G.Entry

-- Region ID
type RegionID = Int
type PageID   = Int

data Region = Region { pid :: PageID
                     , rid :: RegionID
                     } deriving (Show, Eq) 

-- Root region is created automatically with the widget and cannot be destroyed
rootRegion :: RegionID
rootRegion = 0

-- noRegion is the root region's parent
noRegion :: RegionID
noRegion = -1

data CwSelection = CwSelection { selRegion :: Region
                               , selFrom   :: SourcePos
                               , selTo     :: SourcePos
                               }

-- Interface of the widget
data CwAPI = CwAPI {
    pageCreate            :: String                   -- file name
                          -> IO Region,

    regionCreate          :: Region                   -- non-editable parent region
                          -> SourcePos                -- location inside parent region
                          -> Bool                     -- editable?
                          -> String                   -- initial text
                          -> IO ()                    -- region modified callback
                          -> IO Region,

    regionCreateFrom      :: Region                   -- non-editable parent region
                          -> (SourcePos, SourcePos)   -- start/end location inside parent region 
                                                      -- (text between locs becomes sub-region text
                          -> Bool                     -- editable?
                          -> IO ()                    -- region modified callback
                          -> IO Region,               -- result: Created region

    regionDelete          :: Region -> IO (),         -- delete specified region. NOTE: cannot delete rootRegion

    regionGetText         :: Region -> IO String,     

    regionSetText         :: Region -> String -> IO (),

    regionGetBoundedText  :: Region -> (SourcePos, SourcePos) -> IO String,
    
    regionDeleteText      :: Region -> (SourcePos, SourcePos) -> IO (),

    regionInsertText      :: Region                   -- target region
                          -> String
                          -> IO (),
                      
    regionGetAllText      :: Region -> IO String,
    
    tagNew                :: Region -> IO G.TextTag,            -- allocate new tag.  Normal GTK functions can be used to configure associated FG and BG colors
    regionApplyTag        :: Region -> G.TextTag -> (SourcePos, SourcePos) -> IO (), -- Apply tag to coordinates within the region
    regionRemoveTag       :: Region -> G.TextTag -> IO (),
    regionSetMark         :: Region -> G.TextMark -> SourcePos -> IO (),
    regionGetIter         :: Region -> SourcePos -> IO G.TextIter,
    regionGetSelection    :: Region -> IO (Maybe CwSelection),
    regionScrollToPos     :: Region -> SourcePos -> IO (),
    regionEditable        :: Region -> Bool -> IO (),
    dumpRegions           :: Region -> IO ()
}

-- RegionContext - contextual info for each region
data RegionContext = RegionContext {
      rcRegion    :: RegionID,
      rcParent    :: RegionID,
      rcPage      :: PageID,
      rcEditable  :: Bool,
      rcStart     :: G.TextMark,
      rcEnd       :: G.TextMark,
      rcInsert    :: Maybe G.TextMark,
      rcStartPos  :: SourcePos,
      rcCallBack  :: Maybe ( IO ()),
      rcInitWidth :: Int,
      rcInitHeight:: Int,
      rcBgTag     :: G.TextTag
}

instance Eq RegionContext where
    (==) (RegionContext _ p1 _ _ _ _ _ sp1  _ _ _ _) (RegionContext _ p2 _ _ _ _ _ sp2  _ _ _ _)  =  
          if' (p1 == p2) (sp1 == sp2)  False

instance Ord RegionContext where
    compare (RegionContext _ p1 _ _ _ _ _ sp1  _ _ _ _) (RegionContext _ p2 _ _ _ _ _ sp2  _ _ _ _) = 
          if' (p1 == p2) (compare sp1 sp2) (compare p1 p2)


data PageContext =  PageContext {
    pgID          :: PageID,
    pgView        :: G.SourceView,
    pgBuffer      :: G.SourceBuffer,
    pgEditTag     :: G.TextTag,
    pgTagTable    :: G.TextTagTable,
    pgNextRegion  :: RegionID,
    pgRegions     :: [RegionContext],
    pgFileName    :: String
}

type CwRef = (PageContext, RegionContext)

data CodeView = CodeView {
    cvLanguage    :: G.SourceLanguage,
    cvFont        :: G.FontDescription,
    cvPages       :: [PageContext],
    cvNotebook    :: G.Notebook,
    cvSBar        :: CwSBar,
    cvNextPage    :: Int
}

-- Code - tupple containing a CwAPI and a CwView, api and view for a CodeWidget Instance
data Code = Code {
    codeApi     :: CwAPI,
    codeWidget  :: G.Widget,
    codePos     :: G.Widget
}

-- wrapped version
type RCodeView = IORef CodeView

