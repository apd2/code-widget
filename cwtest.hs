-- Test file for the SourceView widget.
module Main where

import Graphics.UI.Gtk
import CodeWidget
--import Text.Parsec
import Text.Parsec.Pos

{-- Magic Block #1
  {}   Inside the braces is the 1st editable region, second is below. Ellipsis passed as text arg to regionCreate. 
--}
fnm = "cwtest.hs"
magicPos1 = newPos   fnm 10 4
hlite1_start = newPos fnm 18 10
hlite1_end = newPos   fnm 18 60

{-- MB#2 This magic block is used to test regionCreateFrom - ellipsis moves from root region to sub-region
  {...}  This text should highlight when hilite1 is pressed
--}

  -- This text should highlight when hilite2 is pressed ... out to here
magicPos2 = ((newPos fnm 18 4), (newPos fnm 18 7))
hlite2_start = newPos fnm 21 3
hlite2_end = newPos fnm 21 72

magic1braced = ((newPos fnm 10 3),(newPos fnm 10 5))
magic2braced = ((newPos fnm 18 3),(newPos fnm 18 8))

colorCont = "#80c080"
insText = "this text is inserted"

main = do
  _ <- initGUI
  win <- windowNew
  windowSetPosition win WinPosCenter
  _ <- win `onDestroy` mainQuit


  -- create a new CodeWidget
  cd <- codeWidgetNew "haskell"  800 800 
  let c =  codeApi cd
  let sv = codeWidget cd
  let sp = codePos cd

  rr <- pageCreate c fnm

  rr2 <- pageCreate c "CodeWidgetAPI.hs"
  rr3 <- pageCreate c "CodeWidgetInternal.hs"

  --get some tags to highlite it with
  htag1 <- tagNew c rr
  set htag1 [textTagBackground := colorCont]
  htag2 <- tagNew c rr
  set htag2 [textTagBackground := colorCont]

  -- put CodeWidget in a scrolled window
  --sw <- scrolledWindowNew Nothing Nothing
  --sw `containerAdd` sv
  --scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
  --sw `scrolledWindowSetShadowType` ShadowIn

  -- Add some buttons to drive tests
  vbox <- vBoxNew False 0
  widgetShow vbox
  boxPackStart vbox sv PackGrow 0
  bbox <- hButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  widgetShow bbox
  boxPackStart vbox bbox PackNatural 0
  boxPackEnd  vbox sp PackNatural 0

  b1 <- buttonNewWithLabel "hilite 1"
  b2 <- buttonNewWithLabel "unhilite 1"
  b3 <- buttonNewWithLabel "GetText root"
  b4 <- buttonNewWithLabel "hilite 2"
  b5 <- buttonNewWithLabel "unhilite 2"
  b6 <- buttonNewWithLabel "GetText 2"
  b7 <- buttonNewWithLabel "Move Selection"
  b8 <- buttonNewWithLabel "Dump Regions"
  widgetShow b1
  widgetShow b2
  widgetShow b3
  widgetShow b4
  widgetShow b5
  widgetShow b6
  widgetShow b7
  widgetShow b8
  containerAdd bbox b1
  containerAdd bbox b2
  containerAdd bbox b3
  containerAdd bbox b4
  containerAdd bbox b5
  containerAdd bbox b6
  containerAdd bbox b7
  containerAdd bbox b8
  _ <- on b1 buttonActivated (regionApplyTag c rr htag1 (hlite1_start, hlite1_end))
  _ <- on b2 buttonActivated (regionRemoveTag c rr htag1)
  _ <- on b3 buttonActivated (rgnText c rr)
  _ <- on b4 buttonActivated (regionApplyTag c rr htag2 (hlite2_start, hlite2_end))
  _ <- on b5 buttonActivated (regionRemoveTag c rr htag2)
  _ <- on b8 buttonActivated (dumpRs c rr)
  --_ <- on b8 buttonActivated (regionScrollToPos c rr hlite2_start)

  win `containerAdd` vbox

  -- show the widget and run the main loop
  windowSetDefaultSize win 600 600
  widgetShowAll win
  
  -- create two regions corresponding 
  --putStrLn "creating mb1 region"
  mb1 <- regionCreateFrom c rr magic1braced False (chgreg 1)
  --putStrLn $ "creating ed1 subregion from mb1: " ++ show mb1
  ed1 <- regionCreate c mb1 (newPos fnm 1 2) True "..." (chgreg 2)
  --putStrLn $ "created ed1 region " ++ (show ed1)
  mb2 <- regionCreateFrom c rr magic2braced False (chgreg 3)
  ed2 <- regionCreateFrom c mb2 ((newPos fnm 1 2),(newPos fnm 1 5)) True (chgreg 4)
  --putStrLn $ "create region  " ++ (show ed2)
  _ <- on b7 buttonActivated (moveIt c mb2)
  _ <- on b6 buttonActivated (rgnText c ed2)

  --putStrLn "calling mainGUI"

  mainGUI

boohoo :: IO ()
boohoo = return ()

chgreg :: Int -> IO ()
chgreg id = do putStrLn $ "RegionChange: " ++ show id

moveIt :: CwAPI -> CodeWidget.Region -> IO ()
moveIt c tr = do
    sel <- regionGetSelection c tr
    case sel of 
          Nothing -> return ()
          Just sx -> do let r = selRegion sx
                        let f = selFrom   sx
                        let t = selTo     sx
                        txt <- regionGetBoundedText c r (f,t)
                        regionDeleteText c r (f, t)
                        --putStrLn $ "moveIt: R: " ++ show r ++ " F:" ++ show f ++ " T:" ++ show t
                        regionInsertText c tr txt
                

dumpRs :: CwAPI -> CodeWidget.Region -> IO ()
dumpRs c r = do putStrLn "Dump of All Regions"
                dumpRegions c r

rgnText :: CwAPI -> CodeWidget.Region -> IO ()
rgnText c r = do t <- regionGetText c r
                 putStrLn $ "getText Region=" ++ show r
                 putStrLn t
                 --dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk t
                 --_ <- onResponse dialog (\_ -> widgetDestroy dialog)
                 --windowPresent dialog
                 
  
