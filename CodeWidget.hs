module CodeWidget (codeWidgetNew,
                   Code(..), 
                   CwAPI(..),
                   CwSelection(..),
                   Region, 
                   rootRegion, 
                   CwView, 
                   CwIter ) where

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


import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Text.Parsec
import Text.Parsec.Pos
import Data.List
--import Data.Maybe
import Data.IORef
import Control.Monad
import Util


--------------------------------------------------------------
-- Constants
--------------------------------------------------------------
fontSrc    = "Courier 10 Pitch"
--fontSrc    = "Monospace 9 Pitch"


--------------------------------------------------------------
-- Types
--------------------------------------------------------------
--encapsulate types from gtk
type CwView  = G.SourceView
type CwIter  = G.TextIter

-- Region ID
type Region = Int

-- Root region is created automatically with the widget and cannot be destroyed
rootRegion :: Region
rootRegion = 0

-- noRegion is the root region's parent
noRegion :: Region
noRegion = -1

data CwSelection = CwSelection { selRegion :: Region
                               , selFrom   :: SourcePos
                               , selTo     :: SourcePos
                               }

-- Interface of the widget
data CwAPI = CwAPI {
    regionCreate          :: Region                   -- non-editable parent region
                          -> SourcePos                -- location inside parent region
                          -> Bool                     -- editable?
                          -> String                   -- initial text
                          -> IO Region,

    regionCreateFrom      :: Region                   -- non-editable parent region
                          -> (SourcePos, SourcePos)   -- start/end location inside parent region 
                                                      -- (text between locs becomes sub-region text
                          -> Bool                     -- editable?
                          -> IO Region,               -- result: Created region

    regionDelete          :: Region -> IO (),         -- delete specified region. NOTE: cannot delete rootRegion

    regionGetText         :: Region -> IO String,     

    regionSetText         :: Region -> String -> IO (),

    regionGetBoundedText  :: Region -> (SourcePos, SourcePos) -> IO String,
    
    regionDeleteText      :: Region -> (SourcePos, SourcePos) -> IO (),

    regionInsertText      :: Region                   -- target region
                          -> String
                          -> IO (),
                      
    getAllText            :: IO String,
    
    tagNew                :: IO G.TextTag,            -- allocate new tag.  Normal GTK functions can be used to configure associated FG and BG colors
    regionApplyTag        :: Region -> G.TextTag -> (SourcePos, SourcePos) -> IO (), -- Apply tag to coordinates within the region
    regionRemoveTag       :: Region -> G.TextTag -> IO (),
    regionSetMark         :: Region -> G.TextMark -> SourcePos -> IO (),
    regionGetIter         :: Region -> SourcePos -> IO G.TextIter,
    regionGetSelection    :: IO (Maybe CwSelection),
    dumpRegions           :: IO ()
}

-- RegionContext - contextual info for each region
data RegionContext = RegionContext {
      rcRegion    :: Region,
      rcParent    :: Region,
      rcEditable  :: Bool,
      rcStart     :: G.TextMark,
      rcEnd       :: G.TextMark,
      rcInsert    :: Maybe G.TextMark,
      rcStartPos  :: SourcePos,
      rcInitWidth :: Int,
      rcInitHeight:: Int
}

instance Eq RegionContext where
    (==) (RegionContext _ p1 _ _ _ _ sp1 _ _) (RegionContext _ p2 _ _ _ _ sp2 _ _)  =  if' (p1 == p2) (sp1 == sp2)  False

instance Ord RegionContext where
    compare (RegionContext _ p1 _ _ _ _ sp1 _ _) (RegionContext _ p2 _ _ _ _ sp2 _ _) = if' (p1 == p2) (compare sp1 sp2) (compare p1 p2)


-- CodeView - GTK TextView/SourceView context to support Widget API
data CodeView =  CodeView {
    cvView        :: G.SourceView,
    cvBuffer      :: G.SourceBuffer,
    cvEditTag     :: G.TextTag,
    cvTagTable    :: G.TextTagTable,
    cvNextRegion  :: Region,
    cvRegions     :: [RegionContext],
    cvFileName    :: String
}

-- Code - tupple containing a CwAPI and a CwView, api and view for a CodeWidget Instance
data Code = Code {
    api     :: CwAPI,
    view    :: CwView
}

-- wrapped version
type RCodeView = IORef CodeView

dbgPrints :: Bool
dbgPrints = False
--dbgPrints = True
mpStrLn :: String -> IO ()
mpStrLn s = do case dbgPrints of
                    True  -> mpStrLn s
                    False -> return ()
-- API creation

{-- codeWidgetNew - create an instance of the code widget.
              codeWidgetNew :: String       -- language type (file extension)
                            -> Maybe String -- file name (optional)
                            -> Int       -- width
                            -> Int       -- height
                            -> IO Code
--}
codeWidgetNew :: String -> Maybe String -> Int -> Int -> IO Code
codeWidgetNew l f w h = do
    slm <- G.sourceLanguageManagerGetDefault
    mlng <- G.sourceLanguageManagerGetLanguage slm l
    lng <- case mlng of
              Nothing -> error $ "Can't find " ++ l ++ " Language Definition"
              Just x  -> return x

    table <- G.textTagTableNew
    buf <- G.sourceBufferNew (Just table)
    etag <- G.textTagNew Nothing
    G.set etag [G.textTagEditable G.:= False]
    G.textTagTableAdd table etag

    G.sourceBufferSetLanguage buf (Just lng)
    G.sourceBufferSetHighlightSyntax buf True
    v <- G.sourceViewNewWithBuffer buf
    font <- G.fontDescriptionFromString fontSrc
    
    G.widgetModifyFont v $ Just font
    G.widgetSetSizeRequest v w h
    G.textViewSetEditable v True
    root <- mkRootRegion buf
    ref <- newIORef $ CodeView { cvView       = v
                               , cvBuffer     = buf
                               , cvTagTable   = table
                               , cvEditTag    = etag
                               , cvNextRegion = (rootRegion + 1)
                               , cvRegions    = [root]
                               , cvFileName   = case f of
                                                     Nothing -> ""
                                                     Just s  -> s
                               }

    return $ Code { api = CwAPI { regionCreate          = codeRegionCreate     ref
                                , regionCreateFrom      = codeRegionCreateFrom ref
                                , regionDelete          = codeRegionDelete     ref
                                , regionGetText         = codeRegionGetText    ref
                                , regionGetBoundedText  = codeRegionGetBoundedText   ref
                                , regionSetText         = codeRegionSetText    ref
                                , regionDeleteText      = codeRegionDeleteText ref
                                , regionInsertText      = codeRegionInsertText ref
                                , getAllText            = codeGetAllText       ref
                                , tagNew                = codeTagNew           ref
                                , regionApplyTag        = codeRegionApplyTag   ref
                                , regionRemoveTag       = codeRegionRemoveTag  ref
                                , regionSetMark         = codeRegionSetMark    ref
                                , regionGetIter         = codeRegionGetIter    ref
                                , regionGetSelection    = codeRegionGetSelection ref
                                , dumpRegions           = codeDumpRegions      ref
                                }
                  , view = v
                  }

-- Individual API functions

codeRegionCreate :: RCodeView -> Region -> SourcePos -> Bool -> String -> IO Region
codeRegionCreate ref parent pos ed txt = do
    cv <- readIORef ref
    case cvGetRegion cv parent of
          Nothing -> error ("regionCreate: cannot find parent region " ++ (show parent))
          Just rc  -> do r <- cvRgnCreateEmpty ref rc pos ed
                         codeRegionSetText ref r txt
                         return r
                      
codeRegionCreateFrom :: RCodeView -> Region -> (SourcePos, SourcePos) -> Bool -> IO Region
codeRegionCreateFrom ref parent (from, to) ed = do
    cv <- readIORef ref
    case cvGetRegion cv parent of
          Nothing -> error ("regionCreateFrom: cannot find parent region " ++ (show parent))
          Just rc -> cvRgnCreateFrom ref rc from to ed False
                      

codeRegionDelete :: RCodeView -> Region -> IO ()
codeRegionDelete ref r = do
    cv <- readIORef ref
    if r > 0 
        then case cvGetRegion cv r of 
                  Nothing -> error ("regionDelete: specified region does not exist: " ++ (show r))
                  Just x  -> do let newrgns = cvOtherRegions cv r
                                G.textBufferDeleteMark (cvBuffer cv) (rcStart x)
                                G.textBufferDeleteMark (cvBuffer cv) (rcEnd x)
                                writeIORef ref cv {cvRegions = newrgns}
        else if' (r == 0) (error "regionDelete: attempt to delete root region!") (error $ "regionDelete: invalid negative region " ++ (show r))
    cvSetEditFlags cv 


codeRegionGetText :: RCodeView -> Region -> IO String
codeRegionGetText ref r = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionGetText: region not found: " ++ (show r))
            Just x  -> cvSubRgnText cv x


codeRegionGetBoundedText :: RCodeView -> Region -> (SourcePos, SourcePos) -> IO String
codeRegionGetBoundedText ref r (from, to) = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionGetText: region not found: " ++ (show r))
            Just x  -> do s <- cvRgnMapPos cv x from
                          e <- cvRgnMapPos cv x to
                          si <- rootIterFromPos cv s
                          ei <- rootIterFromPos cv e
                          cvRgnGetText cv si ei False



codeRegionSetText :: RCodeView -> Region -> String -> IO ()
codeRegionSetText ref r txt = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionGetText: region not found: " ++ (show r))
            Just x  -> if (cvIsRoot x) 
                          then do G.textBufferSetText (cvBuffer cv) txt
                                  
                          else do iter1 <- G.textBufferGetIterAtMark (cvBuffer cv) (rcStart x)
                                  iter2 <- G.textBufferGetIterAtMark (cvBuffer cv) (rcEnd x)
                                  G.textBufferDelete (cvBuffer cv) iter1 iter2
                                  G.textBufferInsert (cvBuffer cv) iter1 txt
    cvSetEditFlags cv 

codeRegionInsertText :: RCodeView -> Region -> String -> IO ()
codeRegionInsertText ref r t = do
    cv <- readIORef ref  
    case cvGetRegion cv r of 
            Nothing -> error ("regionInsertText: region not found: " ++ (show r))
            Just x  -> do di  <- cvInsertMark ref x
                          i3  <- G.textBufferGetIterAtMark (cvBuffer cv) di
                          cvRgnInsertText cv i3 t
                          cvSetEditFlags cv 

codeRegionDeleteText :: RCodeView -> Region -> (SourcePos, SourcePos) -> IO ()
codeRegionDeleteText ref r (from, to) = do
    cv <- readIORef ref  
    case cvGetRegion cv r of 
            Nothing -> error ("regionDeleteText: region not found: " ++ (show r))
            Just x  -> do mpStrLn $ "deleteText: F:" ++ show from ++ " T:" ++ show to
                          s <- cvRgnMapPos cv x from
                          e <- cvRgnMapPos cv x to
                          si <- rootIterFromPos cv s
                          ei <- rootIterFromPos cv e
                          G.textBufferDelete (cvBuffer cv) si ei
    
codeGetAllText :: RCodeView -> IO String
codeGetAllText ref = do
    cv <- readIORef ref
    cvGetAllText cv


codeTagNew :: RCodeView -> IO G.TextTag
codeTagNew ref = do 
    cv  <- readIORef ref
    tag <- G.textTagNew Nothing
    G.textTagTableAdd (cvTagTable cv) tag
    return tag


codeRegionApplyTag :: RCodeView -> Int -> G.TextTag -> (SourcePos, SourcePos) -> IO ()
codeRegionApplyTag ref r t (from, to) = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionApplyTag: region not found: " ++ (show r))
            Just x -> do rfrom <- cvRgnMapPos cv x from
                         rto   <- cvRgnMapPos cv x to
                         siter <- rootIterFromPos cv rfrom
                         eiter <- rootIterFromPos cv rto
                         G.textBufferApplyTag (cvBuffer cv) t siter eiter


codeRegionRemoveTag :: RCodeView -> Int -> G.TextTag -> IO ()
codeRegionRemoveTag ref r t = do
    cv <- readIORef ref
    case cvGetRegion cv r of 
            Nothing -> error ("regionRemoveTag: region not found: " ++ (show r))
            Just x -> do iter1 <- cvRgnStart cv x
                         iter2 <- cvRgnEnd cv x
                         G.textBufferRemoveTag (cvBuffer cv) t iter1 iter2


codeRegionSetMark :: RCodeView -> Region -> G.TextMark -> SourcePos -> IO ()
codeRegionSetMark ref r m p = do
    cv <- readIORef ref
    case cvGetRegion cv r of
            Nothing -> error ("regionSetMark: region not found: " ++ (show r))
            Just x  -> do rpos <- cvRgnMapPos cv x p
                          apos <- cvAllowForPriorSubs cv x rpos
                          iter <- G.textBufferGetIterAtLineOffset (cvBuffer cv) (sourceLine apos - 1) (sourceColumn apos - 1) 
                          G.textBufferAddMark (cvBuffer cv) m iter

codeRegionGetIter :: RCodeView -> Region -> SourcePos -> IO G.TextIter
codeRegionGetIter ref r p = do
    cv <- readIORef ref
    case cvGetRegion cv r of
            Nothing -> error ("regionGetIter: region not found: " ++ (show r))
            Just x  -> do rpos <- cvRgnMapPos cv x p
                          apos <- cvAllowForPriorSubs cv x rpos
                          G.textBufferGetIterAtLineOffset (cvBuffer cv) (sourceLine apos - 1) (sourceColumn apos - 1)
                        
codeRegionGetSelection :: RCodeView ->  IO (Maybe CwSelection)
codeRegionGetSelection ref = do
    cv <- readIORef ref
    hassel <- G.textBufferHasSelection (cvBuffer cv)
    case hassel of 
          False -> return Nothing
          True  -> do (ifm,ito) <- G.textBufferGetSelectionBounds (cvBuffer cv)
                      pfm <- posFromIter cv ifm
                      pto <- posFromIter cv ito
                      mpStrLn $ "getSelection: From:" ++ show pfm ++ " To:" ++ show pto
                      mrc <- cvWhoHoldsPos cv pfm
                      case mrc of 
                            Nothing -> return Nothing
                            Just rc -> do sp <- cvMapPosToRgn cv rc pfm
                                          ep <- cvMapPosToRgn cv rc pto
                                          mpStrLn $ "getSel: R:" ++ show (rcRegion rc) ++ " ST:" ++ show sp ++ " ED:" ++ show ep
                                          return $ Just (CwSelection (rcRegion rc) sp ep)
                      
                   


codeDumpRegions :: RCodeView -> IO ()
codeDumpRegions ref = do
    cv <- readIORef ref
    mapM_ (\r -> do t <- dumpRgn cv r
                    putStrLn t) (cvRegions cv)

-- Helper functions

cvRgnCreateEmpty :: RCodeView -> RegionContext -> SourcePos -> Bool -> IO Region
cvRgnCreateEmpty ref parent from ed =  do 
    cv <- readIORef ref
    let nextr = cvNextRegion cv
    let currgns = cvRegions cv
    smk <- newLeftMark
    emk <- newRightMark
    rfm <- cvRgnMapPos cv parent from
    mpStrLn $ "cvRgnCreateEmpty: " ++ show rfm
    itfm <- rootIterFromPos cv rfm
    G.textBufferAddMark (cvBuffer cv) smk itfm
    G.textBufferAddMark (cvBuffer cv) emk itfm
    let newrgn = RegionContext { rcRegion     = nextr
                               , rcParent     = rcRegion parent
                               , rcEditable   = ed
                               , rcStart      = smk
                               , rcEnd        = emk
                               , rcInsert     = Nothing
                               , rcStartPos   = rfm
                               , rcInitWidth  = 0
                               , rcInitHeight = 0
                               }
    writeIORef ref cv { cvNextRegion =  nextr + 1, cvRegions = newrgn:currgns }
    return nextr

--Create new region from existing region - supports 3 cases: new/parent, parent/new, and parent/new/parent. 
--In addition, if not 
cvRgnCreateFrom :: RCodeView -> RegionContext -> SourcePos -> SourcePos -> Bool -> Bool ->IO Region
cvRgnCreateFrom ref rc from to ed sib =  do 
    cv <- readIORef ref
    let nextr = cvNextRegion cv
    let othrgns = cvOtherRegions cv (rcRegion rc)
    let par = rcRegion rc
    smk <- newLeftMark
    emk <- newRightMark
    rfm <- cvRgnMapPos cv rc from
    rto <- cvRgnMapPos cv rc to
    st  <- cvRgnStartPos cv rc
    nd  <- cvRgnEndPos cv rc
    let h = (sourceLine rto) - (sourceLine rfm)
    let c = (sourceColumn rto) - (sourceColumn rfm)
    if st == rfm 
        then  do {-- if startpos == the parent region start, then the new region
                  goes ahead of the current region the new region gets the parent's
                  rcStart and rcStartPos.  --} 
              mpStrLn $ "cvRgnCreateFrom<head> : " ++ show rfm ++ " " ++ show rto
              itto <- rootIterFromPos cv rto
              G.textBufferAddMark (cvBuffer cv) smk itto  -- new start mark for parent
              G.textBufferAddMark (cvBuffer cv) emk itto  -- new end mark for new region
              let newrgn = RegionContext { rcRegion   = nextr
                                         , rcParent   = if' (sib == False) par (rcParent rc)
                                         , rcEditable = ed
                                         , rcStart    = (rcStart rc)
                                         , rcEnd      = emk
                                         , rcInsert   = Nothing
                                         , rcStartPos = (rcStartPos rc)
                                         , rcInitWidth = c
                                         , rcInitHeight = h
                                         }
              let newPar =  rc { rcStart    = smk
                               , rcStartPos = rfm
                               }
              let newcv = cv { cvNextRegion =  nextr + 1, cvRegions = newrgn:newPar:othrgns }
              writeIORef ref newcv
              cvSetEditFlags newcv
              return nextr
        else if nd == rto
                then  do {-- If endpos = parent's endpos, new region goes after parent. new region
                          gets parent's rcEnd, parent gets new rcEnd from newregion's startpos --}
                      mpStrLn $ "cvRgnCreateFrom<tail> : " ++ show rfm ++ " " ++ show rto
                      itfm <- rootIterFromPos cv rfm
                      G.textBufferAddMark (cvBuffer cv) smk itfm
                      G.textBufferAddMark (cvBuffer cv) emk itfm
                      let newrgn = RegionContext { rcRegion   = nextr
                                                 , rcParent   = if' (sib == False) par (rcParent rc)
                                                 , rcEditable = ed
                                                 , rcStart    = smk
                                                 , rcEnd      = (rcEnd rc)
                                                 , rcInsert   = Nothing
                                                 , rcStartPos = rfm
                                                 , rcInitWidth = c
                                                 , rcInitHeight = h
                                                 }
                      let newPar = rc { rcEnd = emk}

                      let newcv = cv { cvNextRegion =  nextr + 1, cvRegions = newrgn:newPar:othrgns }
                      writeIORef ref newcv
                      cvSetEditFlags newcv
                      return nextr
                else  do -- Nothing special - parent area becomes new subregion
                      mpStrLn $ "cvRgnCreateFrom<emb> : " ++ show rfm ++ " " ++ show rto
                      itfm <- rootIterFromPos cv rfm
                      itto <- rootIterFromPos cv rto
                      G.textBufferAddMark (cvBuffer cv) smk itfm
                      G.textBufferAddMark (cvBuffer cv) emk itto
                      let newrgn = RegionContext { rcRegion   = nextr
                                                 , rcParent   = par
                                                 , rcEditable = ed
                                                 , rcStart    = smk
                                                 , rcEnd      = emk
                                                 , rcInsert   = Nothing
                                                 , rcStartPos = rfm
                                                 , rcInitWidth = c
                                                 , rcInitHeight = h
                                                 }
                      let newcv = cv { cvNextRegion =  nextr + 1, cvRegions = newrgn:rc:othrgns }
                      writeIORef ref newcv
                      cvSetEditFlags newcv
                      return nextr

-- get a list of all regions except the specified one
cvOtherRegions :: CodeView -> Region -> [RegionContext]
cvOtherRegions cv r = filter (\x -> r /= (rcRegion x)) $ cvRegions cv

-- get a list if all subregions of this region
cvChildRegions :: CodeView -> RegionContext -> [RegionContext]
cvChildRegions cv r = filter (\x -> rcRegion r == (rcParent x)) $ cvRegions cv

-- get the specified region, maybe
cvGetRegion :: CodeView -> Region -> Maybe RegionContext
cvGetRegion cv r = 
    let rgs = filter (\x -> r == (rcRegion x)) $ cvRegions cv
    in case rgs of 
            [] -> Nothing
            _  -> Just $ head rgs

-- handle GTK "editable" marking for all regions
cvSetEditFlags :: CodeView -> IO ()
cvSetEditFlags cv = do
    case cvGetRegion cv rootRegion of
         Nothing -> error "no root region"
         Just r  -> cvRgnStatic cv r
    mapM_ (cvRgnEditable cv) (cvEditableRgns cv)

-- return a list of all editable regions
cvEditableRgns :: CodeView -> [RegionContext]
cvEditableRgns cv = filter rcEditable (cvRegions cv)


-- setup GTK goodies to make a region static
cvRgnStatic :: CodeView -> RegionContext -> IO ()
cvRgnStatic cv rc = do
    si <- cvRgnStart cv rc
    ei <- cvRgnEnd   cv rc
    let buf = cvBuffer cv
    let tag = cvEditTag cv
    G.textBufferRemoveTag buf tag si ei
    G.textBufferApplyTag buf tag si ei 

-- setup GTK goodies to make a region editable 
cvRgnEditable :: CodeView -> RegionContext -> IO ()
cvRgnEditable cv rc = do
    si <- cvRgnStart cv rc
    ei <- cvRgnEnd   cv rc
    G.textBufferRemoveTag (cvBuffer cv) (cvEditTag cv) si ei

-- Get a TextIter set to the start of the region
cvRgnStart :: CodeView -> RegionContext -> IO G.TextIter
cvRgnStart cv rc = do
    G.textBufferGetIterAtMark (cvBuffer cv) (rcStart rc)

-- Get a TextIter set to the end of the region
cvRgnEnd :: CodeView -> RegionContext -> IO G.TextIter
cvRgnEnd cv rc = do
    G.textBufferGetIterAtMark ( cvBuffer cv) (rcEnd rc)

-- convert a TextIter into a SourcePos
posFromIter :: CodeView -> G.TextIter -> IO SourcePos
posFromIter cv iter = do
    l <- G.textIterGetLine iter
    c <- G.textIterGetLineOffset iter
    return $ newPos (cvFileName cv) (l + 1)  (c + 1)

-- check if a given root-relative position is contained in the specified sub-region
cvRgnPosInside :: CodeView -> RegionContext -> SourcePos -> IO Bool
cvRgnPosInside cv rc pos = do
    sp <- cvRgnStartPos cv rc
    ep <- cvRgnEndPos cv rc
    return $ if' (sp <= pos && ep >= pos) True False

-- convert a root-relative position to a subregion-relative position
cvMapPosToRgn :: CodeView -> RegionContext -> SourcePos -> IO SourcePos
cvMapPosToRgn cv rc pos = do
    sp <- cvRgnStartPos cv rc
    return $ newPos (cvFileName cv) ((sourceLine pos) - (sourceLine sp) + 1) ((sourceColumn pos) - (sourceColumn sp) + 1)

-- get region's current starting SourcePos - 
cvRgnStartPos :: CodeView -> RegionContext -> IO SourcePos
cvRgnStartPos cv rc = do
    iter <- cvRgnStart cv rc
    posFromIter cv iter

-- get region's initial position - may be different from current starting SourcePos due to edits to preceeding regions
cvRgnInitPos :: CodeView -> RegionContext -> SourcePos
cvRgnInitPos _ rc = rcStartPos rc

-- get region's initial line
cvRgnInitLine cv rc = sourceLine (cvRgnInitPos cv rc)

-- get region's ending SourcePos
cvRgnEndPos :: CodeView -> RegionContext -> IO SourcePos
cvRgnEndPos cv rc = do
    iter <- cvRgnEnd cv rc
    posFromIter cv iter

-- map a Region.SourcePos to a global buffer position - makes adjustments for other subregions
cvRgnMapPos :: CodeView -> RegionContext -> SourcePos -> IO SourcePos
cvRgnMapPos cv rc p = do
    p2 <- cvRgnStartPos cv rc
    let nl = (sourceLine p) + (sourceLine p2) - 1
    let nc = (sourceColumn p) + (sourceColumn p2) - 1
    let np = newPos (sourceName p) nl nc
    cvAllowForPriorSubs cv rc np

-- return the number of lines in a region
cvRgnHeight :: CodeView -> RegionContext -> IO Line
cvRgnHeight cv rc = do
    spos <- cvRgnStartPos cv rc
    epos <- cvRgnEndPos cv rc
    hm <- mapM (cvRgnHeight cv) (cvChildRegions cv rc)
    let ch = foldl (+) 0 hm
    return $ (sourceLine epos) - (sourceLine spos) - (rcInitHeight rc) - ch

-- return the width of a region - the difference bewteen the region's position at creating and its current ending position column
cvRgnWidth :: CodeView -> RegionContext -> IO Column
cvRgnWidth cv rc = do
    spos <- cvRgnStartPos cv rc
    epos <- cvRgnEndPos cv rc
    wm <- mapM (cvRgnWidth cv) (cvChildRegions cv rc)
    let cw = foldl (+) 0 wm
    return $ (sourceColumn epos) - (sourceColumn spos) - (rcInitWidth rc) - cw

-- Get a root-normalized TextIter for the given position
rootIterFromPos :: CodeView -> SourcePos -> IO G.TextIter
rootIterFromPos cv pos = do
    G.textBufferGetIterAtLineOffset (cvBuffer cv)  (sourceLine pos - 1)  (sourceColumn pos - 1)

-- Adjust a region position to account for a preceeding subregion
cvAdjustForSub :: CodeView -> SourcePos -> RegionContext -> IO SourcePos
cvAdjustForSub cv pos rc = do
    let ln = cvRgnInitLine cv rc
    let fn = sourceName pos
    let rn = rcRegion rc
    if rn == rootRegion
        then do return pos
        else if sourceLine pos > ln
                then do h <- cvRgnHeight cv rc
                        let nln = (sourceLine pos) + h
                        let np =  newPos fn nln (sourceColumn pos)
                        mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ "):" ++ show pos ++ " H:" ++ show h
                        return np
                else if sourceLine pos == ln
                        then do w <- cvRgnWidth cv rc
                                h <- cvRgnHeight cv rc
                                let ncol = (sourceColumn pos) + w
                                let np =  newPos fn (h + (sourceLine pos)) ncol
                                mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ "):" ++ show pos ++ " H:" ++ show h ++ " W:" ++ show w
                                return np
                        else do mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ ") N/A :" ++ show ln
                                return pos
            

-- Adjust a SourcePos for any edits done to editable sub-regions
cvAllowForPriorSubs :: CodeView -> RegionContext -> SourcePos -> IO SourcePos
cvAllowForPriorSubs cv rc p = do
    np <- foldM (\rgn -> cvAdjustForSub cv rgn) p (cvChildRegions cv rc)
    return np

-- is specified region the rootRegion?
cvIsRoot :: RegionContext -> Bool
cvIsRoot rc = if' (rcRegion rc == rootRegion) True False

-- create a new left-side mark
newLeftMark :: IO G.TextMark
newLeftMark = do mk <- G.textMarkNew Nothing True
                 G.textMarkSetVisible mk False
                 return mk

-- create a new right-side mark
newRightMark :: IO G.TextMark
newRightMark = do mk <- G.textMarkNew Nothing False
                  G.textMarkSetVisible mk False
                  return mk

-- If region does not yet have an Insertion mark, create one and add it. Otherwise, return the existing one
cvInsertMark :: RCodeView -> RegionContext -> IO G.TextMark
cvInsertMark ref rc = do cv <- readIORef ref
                         case (rcInsert rc) of 
                              Nothing -> do mk <- newRightMark
                                            st <- cvRgnStart cv rc
                                            pos <- posFromIter cv st
                                            mpStrLn $ "cvInsertMark:" ++ show (rcRegion rc) ++ " T:" ++ show pos
                                            G.textBufferAddMark (cvBuffer cv) mk st
                                            let nrc = rc {rcInsert = (Just mk)}
                                            let orc = cvOtherRegions cv (rcRegion nrc)
                                            let ncv = cv {cvRegions = nrc:orc}
                                            writeIORef ref ncv
                                            return mk
                              Just x  -> do return x


    
-- create the initial root region
mkRootRegion :: G.SourceBuffer -> IO RegionContext
mkRootRegion bf = do smk <- newLeftMark
                     emk <- newRightMark
                     i1 <- G.textBufferGetStartIter bf
                     i2 <- G.textBufferGetStartIter bf
                     G.textBufferAddMark bf smk i1 
                     G.textBufferAddMark bf emk i2 
                     let pos = newPos "" 1 1
                     let r =  RegionContext  { rcRegion   = rootRegion
                                             , rcParent   = noRegion
                                             , rcEditable = False
                                             , rcStart    = smk
                                             , rcEnd      = emk 
                                             , rcInsert   = Nothing
                                             , rcStartPos = pos
                                             , rcInitWidth = 0
                                             , rcInitHeight = 0
                                             }
                     return r

-- get contents of entire text buffer
cvGetAllText :: CodeView -> IO String
cvGetAllText cv = do
    case cvGetRegion cv rootRegion of 
          Nothing -> error "cvGetAllText: root region not found"
          Just r  -> do it1 <- cvRgnStart cv r
                        it2 <- cvRgnEnd cv r
                        cvRgnGetText cv it1 it2 False

-- wrapper around G.textBufferGetText - debugging aid
cvRgnGetText :: CodeView -> G.TextIter -> G.TextIter -> Bool -> IO String
cvRgnGetText cv es ee b = do
      spos <- posFromIter cv es
      epos <- posFromIter cv ee
      mpStrLn $ "GET TEXT - S:" ++ (show spos) ++ " E:" ++ (show epos)
      G.textBufferGetText (cvBuffer cv) es ee b

-- wrapper around G.textBufferInsertText - debugging aid
cvRgnInsertText :: CodeView -> G.TextIter -> String -> IO ()
cvRgnInsertText cv es t = do
      spos <- posFromIter cv es
      mpStrLn $ "INSERT TEXT - S:" ++ (show spos) ++ " T:" ++ t
      G.textBufferInsert (cvBuffer cv) es t

-- Get a list of regions which are nested in the specified region
cvSubRegions :: CodeView -> RegionContext -> [RegionContext]
cvSubRegions cv rc = sort $ filter (\x -> (rcParent x) == (rcRegion rc)) (cvRegions cv)

-- Build a string from the gaps between the regions in the list
cvSubRgnGapText :: CodeView -> [RegionContext] -> IO String
cvSubRgnGapText _  []  = do return ""
cvSubRgnGapText _  (_:[]) = do  return ""
cvSubRgnGapText cv (x:xs) = do  es <- cvRgnEnd cv x 
                                let x2 = head xs
                                mpStrLn $ "cvSubRgnGapText: regions:" ++ show (rcRegion x) ++","++ show (rcRegion x2)
                                ee <- cvRgnStart cv x2
                                s1 <- cvRgnGetText cv es ee False
                                s2 <- cvSubRgnGapText cv xs
                                return $ s1 ++ s2

-- Get the text for this region, ignoring text in any sub-regions
cvSubRgnText :: CodeView -> RegionContext -> IO String
cvSubRgnText cv rc = do 
          let sr = cvSubRegions cv rc
          case sr of 
               []       -> do es <- (cvRgnStart cv rc)
                              ee <- cvRgnEnd cv rc
                              cvRgnGetText cv es ee False
               (x:[])   -> do es1 <- cvRgnStart cv rc
                              ee1 <- cvRgnStart cv x  
                              es3 <- cvRgnEnd cv x
                              ee3 <- cvRgnEnd cv rc
                              mpStrLn "cvSubRgnText: single subregion"
                              s1 <- cvRgnGetText cv es1 ee1 False
                              s3 <- cvRgnGetText cv es3 ee3 False
                              return $ s1 ++ s3
               (x:xs)   -> do es1 <- cvRgnStart cv rc
                              ee1 <- cvRgnStart cv x
                              let x2 = last xs
                              es3 <- cvRgnEnd cv x2
                              ee3 <- cvRgnEnd cv rc
                              mpStrLn "cvSubRgnText: multiple subregions"
                              s1 <- cvRgnGetText cv es1 ee1 False
                              s2 <- cvSubRgnGapText cv (x:xs)
                              s3 <- cvRgnGetText cv es3 ee3 False
                              return $ s1 ++ s2 ++ s3

-- loop through all regions to find the one that pos belongs to
cvWhoHoldsPos :: CodeView -> SourcePos -> IO (Maybe RegionContext)
cvWhoHoldsPos cv pos = do cvWhoHoldsPos' cv pos (cvEditableRgns cv)
cvWhoHoldsPos' :: CodeView -> SourcePos -> [RegionContext] -> IO (Maybe RegionContext)
cvWhoHoldsPos' _ _ []  = do return Nothing
cvWhoHoldsPos' cv pos (x:xs) = do
      --rtxt <- dumpRgn cv x
      --mpStrLn $ "cvWhoHoldsPos': " ++ rtxt
      ins <- cvRgnPosInside cv x pos
      if' (ins == True) (return $ Just x) (cvWhoHoldsPos' cv pos xs) 

-- debugging: display current state of a region
dumpRgn :: CodeView -> RegionContext -> IO String
dumpRgn cv rgn = do
      let rs = "#:" ++ show (rcRegion rgn) ++ " "
      let ps = "P:" ++ show (rcParent rgn) ++ " "
      let es = "E:" ++ show (rcEditable rgn) ++ " "
      fi <- cvRgnStartPos cv rgn
      let fs = "Fm:" ++ show fi ++ " "
      ti <- cvRgnEndPos cv rgn
      let ts = "To:" ++ show ti ++ " "
      let ss = "StPos:" ++ show (rcStartPos rgn) ++ " "
      let ws = "IW:" ++ show (rcInitWidth rgn) ++ " "
      let hs = "IH:" ++ show (rcInitHeight rgn) ++ " "
      return $ rs ++ ps ++ es ++ fs ++ ts ++ ss ++ ws ++ hs

