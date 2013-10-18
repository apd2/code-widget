module CodeWidgetInternal where

import qualified Graphics.UI.Gtk            as G
import qualified Graphics.UI.Gtk.SourceView as G
import Text.Parsec
import Text.Parsec.Pos
--import Data.List
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Util
import CodeWidgetTypes
import CodeWidgetUtil

cvCurPage :: CodeView -> IO PageID
cvCurPage cv = do G.notebookGetCurrentPage $ cvNotebook cv

cvSetMyPage :: CodeView -> PageContext -> IO ()
cvSetMyPage cv pg = do let nbpg = cvNotebook cv
                       let pgid = pgID pg
                       G.notebookSetCurrentPage nbpg pgid

rgnChangeCB :: CodeView -> PageContext -> G.TextIter -> IO ()
rgnChangeCB cv pg ti = do ploc <- posFromIter pg ti
                          mrc  <- cvWhoHoldsPos pg ploc
                          case mrc of 
                                Nothing -> return ()
                                Just rc -> case rcCallBack rc of
                                                Nothing -> return ()
                                                Just x  -> do _ <- x
                                                              return ()

-- Individual Signal Handlers
bufSigDeleteRange :: RCodeView -> G.TextIter -> G.TextIter -> IO ()
bufSigDeleteRange ref ifm ito = do cv <- readIORef ref
                                   nbi <- cvCurPage cv
                                   case getContexts cv (Region nbi rootRegion) of
                                        Nothing      -> return ()
                                        Just (pg,rc) -> do rgnChangeCB cv pg ifm

bufSigInsertText :: RCodeView -> G.TextIter -> String -> IO ()
bufSigInsertText ref iter txt = do cv <- readIORef ref
                                   nbi <- cvCurPage cv
                                   case getContexts cv (Region nbi rootRegion) of
                                        Nothing      -> return ()
                                        Just (pg,rc) -> do rgnChangeCB cv pg iter


viewSigPasteClibB :: RCodeView -> IO ()
viewSigPasteClibB ref = do cv <- readIORef ref
                           nbi <- cvCurPage cv
                           case getContexts cv (Region nbi rootRegion) of
                                Nothing      -> return ()
                                Just (pg,rc) -> do cvSetEditFlags pg
                                                   

viewKeyRelease :: RCodeView -> G.EventM G.EKey Bool
viewKeyRelease ref = do cv <- liftIO $ readIORef ref
                        pi <- liftIO $ cvCurPage cv
                        case getPage cv pi of
                             Nothing -> return False
                             Just pg -> do cp  <- liftIO $ cvCursorPos pg
                                           mrc <- liftIO $ cvWhoHoldsPos pg cp
                                           case mrc of 
                                                Nothing -> return False
                                                Just rc -> do emp <- liftIO $ rgnEmpty pg rc
                                                              if emp 
                                                                  then do ks <- G.eventKeyName
                                                                          kv <- G.eventKeyVal
                                                                          let mc = G.keyToChar kv
                                                                          case mc of 
                                                                            Nothing -> if ks == "Return"
                                                                                          then do si <- liftIO $ rgnStart pg rc
                                                                                                  liftIO $ cvRgnInsertText pg si "\n"
                                                                                                  liftIO $ cvRgnEditable pg rc
                                                                                                  return True
                                                                                          else return False
                                                                            Just c  -> do si <- liftIO $ rgnStart pg rc
                                                                                          liftIO $ cvRgnInsertText pg si [c]
                                                                                          liftIO $ cvRgnEditable pg rc
                                                                                          return True
                                                                  else return False 



cvRgnCreateEmpty :: RCodeView -> CwRef -> SourcePos -> Bool -> IO() -> IO Region
cvRgnCreateEmpty ref (pg, parent) from ed f =  do 
    cv <- readIORef ref
    let nextr = pgNextRegion pg
    let rgn = Region (pgID pg) nextr
    let currgns = pgRegions pg
    smk <- newLeftMark
    emk <- newRightMark
    bgt <- G.textTagNew Nothing
    let edepth = eRgnNestDepth pg (rcRegion parent)
    let bcolor = getRgnBgColor edepth
    G.set bgt [G.textTagBackground G.:= bcolor]
    G.textTagTableAdd (pgTagTable pg) bgt
    G.textTagSetPriority bgt 2
    rfm <- rgnMapPos pg parent from
    itfm <- rootIterFromPos pg rfm
    G.textBufferAddMark (pgBuffer pg) smk itfm
    G.textBufferAddMark (pgBuffer pg) emk itfm
    let newrgn = RegionContext { rcRegion     = nextr 
                               , rcPage       = pgID pg
                               , rcParent     = rcRegion parent
                               , rcEditable   = ed
                               , rcStart      = smk
                               , rcEnd        = emk
                               , rcInsert     = Nothing
                               , rcStartPos   = rfm
                               , rcCallBack   = Just f
                               , rcInitWidth  = 0
                               , rcInitHeight = 0
                               , rcBgTag      = bgt
                               }
    let newpg = pg { pgNextRegion = nextr + 1, pgRegions = newrgn:currgns }
    writeIORef ref cv { cvPages = newpg:(cvPages cv) }
    cvSetEditFlags newpg
    return rgn

--Create new region from existing region - supports 3 cases: new/parent, parent/new, and parent/new/parent. 
--In addition, if not 
cvRgnCreateFrom :: RCodeView -> CwRef -> SourcePos -> SourcePos -> Bool -> Bool -> IO () -> IO Region
cvRgnCreateFrom ref (pg,rc) from to ed sib f =  do 
    cv <- readIORef ref
    let nextr = pgNextRegion pg
    let rgn = Region (pgID pg) nextr
    let othrgns = otherRegions pg (rcRegion rc)
    let par = rcRegion rc
    smk <- newLeftMark
    emk <- newRightMark
    bgt <- G.textTagNew Nothing
    let edepth = eRgnNestDepth pg $ if' (sib == False) par (rcParent rc)
    let bcolor = getRgnBgColor edepth
    G.set bgt [ G.textTagBackground G.:= bcolor ]
    G.textTagTableAdd (pgTagTable pg) bgt
    G.textTagSetPriority bgt 2
    rfm <- rgnMapPos pg rc from
    rto <- rgnMapPos pg rc to
    st  <- rgnStartPos pg rc
    nd  <- rgnEndPos pg rc
    let h = (sourceLine rto) - (sourceLine rfm)
    let c = (sourceColumn rto) - (sourceColumn rfm)
    if st == rfm 
        then  do {-- if startpos == the parent region start, then the new region
                  goes ahead of the current region the new region gets the parent's
                  rcStart and rcStartPos.  --} 
              --mpStrLn $ "cvRgnCreateFrom<head> : " ++ show rfm ++ " " ++ show rto
              itto <- rootIterFromPos pg rto
              G.textBufferAddMark (pgBuffer pg) smk itto  -- new start mark for parent
              G.textBufferAddMark (pgBuffer pg) emk itto  -- new end mark for new region
              let newrgn = RegionContext { rcRegion   = nextr
                                         , rcPage     = pgID pg
                                         , rcParent   = if' (sib == False) par (rcParent rc)
                                         , rcEditable = ed
                                         , rcStart    = (rcStart rc)
                                         , rcEnd      = emk
                                         , rcInsert   = Nothing
                                         , rcStartPos = (rcStartPos rc)
                                         , rcCallBack   = Just f
                                         , rcInitWidth = c
                                         , rcInitHeight = h
                                         , rcBgTag      = bgt
                                         }
              let newPar =  rc { rcStart    = smk
                               , rcStartPos = rfm
                               }
              let newpg = pg { pgNextRegion =  nextr + 1, pgRegions = newrgn:newPar:othrgns }
              let ops = otherPages cv (pgID newpg)
              let newcv = cv {cvPages = newpg:ops} 
              writeIORef ref newcv
              cvSetEditFlags newpg
              return rgn
        else if nd == rto
                then  do {-- If endpos = parent's endpos, new region goes after parent. new region
                          gets parent's rcEnd, parent gets new rcEnd from newregion's startpos --}
                      --mpStrLn $ "cvRgnCreateFrom<tail> : " ++ show rfm ++ " " ++ show rto
                      itfm <- rootIterFromPos pg rfm
                      G.textBufferAddMark (pgBuffer pg) smk itfm
                      G.textBufferAddMark (pgBuffer pg) emk itfm
                      let newrgn = RegionContext { rcRegion   = nextr
                                                 , rcPage     = pgID pg
                                                 , rcParent   = if' (sib == False) par (rcParent rc)
                                                 , rcEditable = ed
                                                 , rcStart    = smk
                                                 , rcEnd      = (rcEnd rc)
                                                 , rcInsert   = Nothing
                                                 , rcStartPos = rfm
                                                 , rcCallBack   = Just f
                                                 , rcInitWidth = c
                                                 , rcInitHeight = h
                                                 , rcBgTag      = bgt
                                                 }
                      let newPar = rc { rcEnd = emk}

                      let newpg = pg { pgNextRegion =  nextr + 1, pgRegions = newrgn:newPar:othrgns }
                      let ops = otherPages cv (pgID newpg)
                      let newcv = cv {cvPages = newpg:ops} 
                      writeIORef ref newcv
                      cvSetEditFlags newpg
                      return rgn
                else  do -- Nothing special - parent area becomes new subregion
                      --mpStrLn $ "cvRgnCreateFrom<emb> : " ++ show rfm ++ " " ++ show rto
                      itfm <- rootIterFromPos pg rfm
                      itto <- rootIterFromPos pg rto
                      G.textBufferAddMark (pgBuffer pg) smk itfm
                      G.textBufferAddMark (pgBuffer pg) emk itto
                      let newrgn = RegionContext { rcRegion   = nextr
                                                 , rcPage     = pgID pg
                                                 , rcParent   = par
                                                 , rcEditable = ed
                                                 , rcStart    = smk
                                                 , rcEnd      = emk
                                                 , rcInsert   = Nothing
                                                 , rcStartPos = rfm
                                                 , rcCallBack   = Just f
                                                 , rcInitWidth = c
                                                 , rcInitHeight = h
                                                 , rcBgTag      = bgt
                                                 }
                      let newpg = pg { pgNextRegion =  nextr + 1, pgRegions = newrgn:rc:othrgns }
                      let ops = otherPages cv (pgID newpg) 
                      let newcv = cv {cvPages = newpg:ops} 
                      writeIORef ref newcv
                      cvSetEditFlags newpg
                      return rgn


-- handle GTK "editable" marking for all regions
cvSetEditFlags :: PageContext -> IO ()
cvSetEditFlags pg = do
    case getRegion pg rootRegion of
         Nothing -> error "no root region"
         Just r  -> cvRgnStatic pg r
    mapM_ (cvRgnEditable pg) (editableRgns pg)


-- setup GTK goodies to make a region static
cvRgnStatic :: PageContext -> RegionContext -> IO ()
cvRgnStatic pg rc = do
    si <- rgnStart pg rc
    ei <- rgnEnd   pg rc
    let buf = pgBuffer pg
    let tag = pgEditTag pg
    G.textBufferRemoveTag buf tag si ei
    G.textBufferApplyTag buf tag si ei 
    mapM_ (\x -> do G.textMarkSetVisible (rcStart x) False
                    G.textMarkSetVisible (rcEnd x) False 
                    si <- rgnStart pg x
                    ei <- rgnEnd   pg x
                    G.textBufferRemoveTag (pgBuffer pg) (rcBgTag x) si ei ) (pgRegions pg)

-- setup GTK goodies to make a region editable 
cvRgnEditable :: PageContext -> RegionContext -> IO ()
cvRgnEditable pg rc = do
    si <- rgnStart pg rc
    ei <- rgnEnd   pg rc
    ssi <- dumpIter si
    eei <- dumpIter ei
    mpStrLn $ "    cvRgnEditable: " ++ show (rcRegion rc) ++ " Fm:" ++ ssi ++ " To:" ++ eei
    G.textBufferRemoveTag (pgBuffer pg) (pgEditTag pg) si ei
    G.textBufferApplyTag (pgBuffer pg) (rcBgTag rc) si ei
    G.textMarkSetVisible (rcStart rc) True
    G.textMarkSetVisible (rcEnd rc) True

-- map a Region.SourcePos to a global buffer position - makes adjustments for other subregions
rgnMapPos :: PageContext -> RegionContext -> SourcePos -> IO SourcePos
rgnMapPos pg rc p = do
    p2 <- rgnStartPos pg rc
    let nl = (sourceLine p) + (sourceLine p2) - 1
    let nc = if sourceLine p == 1
                then (sourceColumn p) + (sourceColumn p2) - 1
                else sourceColumn p
    let np = newPos (sourceName p) nl nc
    cvAllowForPriorSubs pg rc np

-- check if a given root-relative position is contained in the specified sub-region
cvRgnPosInside :: PageContext -> RegionContext -> SourcePos -> IO Bool
cvRgnPosInside pg rc pos = do
    sp <- rgnStartPos pg rc
    ep <- rgnEndPos pg rc
    return $ if' (sp <= pos && ep >= pos) True False


-- Adjust a region position to account for a preceeding subregion
-- (returns (delta-y, delta-x), e.g., (0,0) if not adjustment is needed). 
cvAdjustForSub :: PageContext -> SourcePos -> RegionContext -> IO (Int, Int)
cvAdjustForSub pg pos rc = do
    let ln = rgnInitLine pg rc
        col = sourceColumn $ rgnInitPos pg rc
    -- let fn = sourceName pos
    let rn = rcRegion rc
    if rn == rootRegion
        then do return (0,0)
        else if sourceLine pos > ln
                then do h <- rgnHeight pg rc
                        --let nln = (sourceLine pos) + h
                        -- let np =  newPos fn nln (sourceColumn pos)
                        mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ "):" ++ show pos ++ " H:" ++ show h
                        return (h, 0)
                else if sourceLine pos == ln && sourceColumn pos > col
                        then do w <- rgnWidth pg rc
                                h <- rgnHeight pg rc
                                -- let ncol = (sourceColumn pos) + w
                                -- let np =  newPos fn (h + (sourceLine pos)) ncol
                                mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ "):" ++ show pos ++ " H:" ++ show h ++ " W:" ++ show w
                                return (h, w)
                        else do mpStrLn $ "cvAdjustForSub:(" ++ show rn ++ ") N/A :" ++ show ln
                                return (0,0)
            

-- Adjust a SourcePos for any edits done to editable sub-regions
cvAllowForPriorSubs :: PageContext -> RegionContext -> SourcePos -> IO SourcePos
cvAllowForPriorSubs pg rc p = do
    (dy, dx) <- (liftM ((mapSnd sum) . (mapFst sum) . unzip)) $ mapM (cvAdjustForSub pg p) (childRegions pg rc)
    return $ newPos (sourceName p) (sourceLine p + dy) (sourceColumn p + dx)


-- If region does not yet have an Insertion mark, create one and add it. Otherwise, return the existing one
cvInsertMark :: RCodeView -> PageContext -> RegionContext -> IO G.TextMark
cvInsertMark ref pg rc = do cv <- readIORef ref
                            case (rcInsert rc) of 
                                Nothing -> do mk <- newRightMark
                                              st <- rgnStart pg rc
                                              pos <- posFromIter pg st
                                              --mpStrLn $ "cvInsertMark:" ++ show (rcRegion rc) ++ " T:" ++ show pos
                                              G.textBufferAddMark (pgBuffer pg) mk st
                                              let nrc = rc {rcInsert = (Just mk)}
                                              let orc = otherRegions pg (rcRegion nrc)
                                              let npg = pg {pgRegions = nrc:orc}
                                              let orp = otherPages cv (pgID pg)
                                              let ncv = cv { cvPages = npg:orp}
                                              writeIORef ref ncv
                                              return mk
                                Just x  -> do return x


    
-- create the initial root region
mkRootRegion :: PageID -> G.SourceBuffer -> G.TextTagTable -> IO RegionContext
mkRootRegion p b t = do smk <- newLeftMark
                        emk <- newRightMark
                        bgt <- G.textTagNew Nothing
                        vmk <- G.textTagNew Nothing
                        G.set bgt [G.textTagBackground G.:= (getRgnBgColor rootRegion)]
                        G.textTagTableAdd t bgt
                        G.textTagSetPriority bgt 1
                        i1 <- G.textBufferGetStartIter b
                        i2 <- G.textBufferGetStartIter b
                        G.textBufferAddMark b smk i1 
                        G.textBufferAddMark b emk i2 
                        let pos = newPos "" 1 1
                        let r =  RegionContext  { rcRegion   = rootRegion
                                                , rcPage     = p
                                                , rcParent   = noRegion
                                                , rcEditable = False
                                                , rcStart    = smk
                                                , rcEnd      = emk 
                                                , rcInsert   = Nothing
                                                , rcStartPos = pos
                                                , rcCallBack = Nothing
                                                , rcInitWidth = 0
                                                , rcInitHeight = 0
                                                , rcBgTag     = bgt
                                                }
                        return r

-- get contents of entire text buffer
cvGetAllText :: PageContext -> RegionID -> IO String
cvGetAllText pg rgn = do
    case getRegion pg rgn of 
          Nothing -> error "cvGetAllText: region not found"
          Just r  -> do it1 <- rgnStart pg r
                        it2 <- rgnEnd pg r
                        cvRgnGetText pg it1 it2 False

-- wrapper around G.textBufferGetText - debugging aid
cvRgnGetText :: PageContext -> G.TextIter -> G.TextIter -> Bool -> IO String
cvRgnGetText pg es ee b = do
      spos <- posFromIter pg es
      epos <- posFromIter pg ee
      --mpStrLn $ "GET TEXT - S:" ++ (show spos) ++ " E:" ++ (show epos)
      G.textBufferGetText (pgBuffer pg) es ee b

-- wrapper around G.textBufferInsertText - debugging aid
cvRgnInsertText :: PageContext -> G.TextIter -> String -> IO ()
cvRgnInsertText pg es t = do
      spos <- posFromIter pg es
      --mpStrLn $ "INSERT TEXT - S:" ++ (show spos) ++ " T:" ++ t
      G.textBufferInsert (pgBuffer pg) es t

-- Build a string from the gaps between the regions in the list
cvSubRgnGapText :: PageContext -> [RegionContext] -> IO String
cvSubRgnGapText _  []  = do return ""
cvSubRgnGapText _  (_:[]) = do  return ""
cvSubRgnGapText pg (x:xs) = do  es <- rgnEnd pg x 
                                let x2 = head xs
                                --mpStrLn $ "cvSubRgnGapText: regions:" ++ show (rcRegion x) ++","++ show (rcRegion x2)
                                ee <- rgnStart pg x2
                                s1 <- cvRgnGetText pg es ee False
                                s2 <- cvSubRgnGapText pg xs
                                return $ s1 ++ s2

-- Get the text for this region, ignoring text in any sub-regions
cvSubRgnText :: PageContext -> RegionContext -> IO String
cvSubRgnText pg rc = do 
          let sr = subRegions pg rc
          case sr of 
               []       -> do es <- (rgnStart pg rc)
                              ee <- rgnEnd pg rc
                              cvRgnGetText pg es ee False
               (x:[])   -> do es1 <- rgnStart pg rc
                              ee1 <- rgnStart pg x  
                              es3 <- rgnEnd pg x
                              ee3 <- rgnEnd pg rc
                              --mpStrLn "cvSubRgnText: single subregion"
                              s1 <- cvRgnGetText pg es1 ee1 False
                              s3 <- cvRgnGetText pg es3 ee3 False
                              return $ s1 ++ s3
               (x:xs)   -> do es1 <- rgnStart pg rc
                              ee1 <- rgnStart pg x
                              let x2 = last xs
                              es3 <- rgnEnd pg x2
                              ee3 <- rgnEnd pg rc
                              --mpStrLn "cvSubRgnText: multiple subregions"
                              s1 <- cvRgnGetText pg es1 ee1 False
                              s2 <- cvSubRgnGapText pg (x:xs)
                              s3 <- cvRgnGetText pg es3 ee3 False
                              return $ s1 ++ s2 ++ s3

-- loop through all regions to find the one that pos belongs to
cvWhoHoldsPos :: PageContext -> SourcePos -> IO (Maybe RegionContext)
cvWhoHoldsPos pg pos = do cvWhoHoldsPos' pg pos (editableRgns pg)
cvWhoHoldsPos' :: PageContext -> SourcePos -> [RegionContext] -> IO (Maybe RegionContext)
cvWhoHoldsPos' _ _ []  = do return Nothing
cvWhoHoldsPos' pg pos (x:xs) = do
      --rtxt <- dumpRgn cv x
      --mpStrLn $ "cvWhoHoldsPos': " ++ rtxt
      ins <- cvRgnPosInside pg x pos
      if' (ins == True) (return $ Just x) (cvWhoHoldsPos' pg pos xs) 

cvCursorPos :: PageContext -> IO SourcePos
cvCursorPos pg = do
    mk   <- G.textBufferGetInsert (pgBuffer pg)
    iter <- G.textBufferGetIterAtMark (pgBuffer pg) mk
    ln   <- G.textIterGetLine iter
    offs <- G.textIterGetLineOffset iter
    return $ newPos (pgFileName pg) (ln + 1) (offs + 1)
    

dumpIter :: G.TextIter -> IO String
dumpIter ti = do ln <- G.textIterGetLine ti
                 co <- G.textIterGetLineOffset ti
                 return $ "[LN: " ++ show (ln + 1) ++ " CO:" ++ show (co + 1) ++ "]"


-- debugging: display current state of a region
dumpRgn :: PageContext -> RegionContext -> IO String
dumpRgn pg rgn = do
      let rs = "#:" ++ show (rcRegion rgn) ++ " "
      let ps = "P:" ++ show (rcParent rgn) ++ " "
      let es = "E:" ++ show (rcEditable rgn) ++ " "
      fi <- rgnStartPos pg rgn
      let fs = "Fm:" ++ show fi ++ " "
      ti <- rgnEndPos pg rgn
      let ts = "To:" ++ show ti ++ " "
      let ss = "StPos:" ++ show (rcStartPos rgn) ++ " "
      let ws = "IW:" ++ show (rcInitWidth rgn) ++ " "
      let hs = "IH:" ++ show (rcInitHeight rgn) ++ " "
      rh <- rgnHeight pg rgn
      rw <- rgnWidth  pg rgn
      let rhs = "HT:" ++ show rh ++ " "
      let rws = "WD:" ++ show rw ++ " "
      let ed = if' (rcEditable rgn) (show (eRgnNestDepth pg (rcParent rgn))) ""
      return $ rs ++ ps ++ es ++ fs ++ ts ++ ss ++ ws ++ hs ++ rhs ++ rws ++ ed

