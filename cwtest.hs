-- Test file for the SourceView widget.
module Main where

import Graphics.UI.Gtk
import CodeWidget
--import Text.Parsec
import Text.Parsec.Pos

{--
  {}   Inside the braces is the 1st editable region, second is below. Ellipsis passed as text arg to regionCreate. 
--}
fnm = "cwtest.hs"
magicPos1 = newPos   fnm 10 4
hlite1_start = newPos fnm 18 10
hlite1_end = newPos   fnm 18 60

{-- This magic block is used to test regionCreateFrom - ellipsis moves from root region to sub-region
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


  -- load up and display a file
  fileContents <- readFile fnm

  -- create a new CodeWidget
  cd <- codeWidgetNew "haskell" (Just fnm) 800 800 
  let c = api cd
  let sv = view cd

  regionSetText c rootRegion fileContents

  --get some tags to highlite it with
  htag1 <- tagNew c 
  set htag1 [textTagBackground := colorCont]
  htag2 <- tagNew c
  set htag2 [textTagBackground := colorCont]

  -- put CodeWidget in a scrolled window
  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` sv
  scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
  sw `scrolledWindowSetShadowType` ShadowIn

  -- Add some buttons to drive tests
  vbox <- vBoxNew False 0
  widgetShow vbox
  boxPackStart vbox sw PackGrow 0
  bbox <- hButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  widgetShow bbox
  boxPackStart vbox bbox PackNatural 0
  b1 <- buttonNewWithLabel "hilite 1"
  b2 <- buttonNewWithLabel "unhilite 1"
  b3 <- buttonNewWithLabel "GetText 0"
  b4 <- buttonNewWithLabel "hilite 2"
  b5 <- buttonNewWithLabel "unhilite 2"
  b6 <- buttonNewWithLabel "GetText 2"
  b7 <- buttonNewWithLabel "MoveSelTxt2"
  b8 <- buttonNewWithLabel "DumpRegions"
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
  _ <- on b1 buttonActivated ((regionApplyTag c) rootRegion htag1 (hlite1_start, hlite1_end))
  _ <- on b2 buttonActivated ((regionRemoveTag c) rootRegion htag1)
  _ <- on b3 buttonActivated (rgnText c rootRegion)
  _ <- on b4 buttonActivated ((regionApplyTag c) rootRegion htag2 (hlite2_start, hlite2_end))
  _ <- on b5 buttonActivated ((regionRemoveTag c) rootRegion htag2)
  _ <- on b8 buttonActivated (dumpRegions c)

  win `containerAdd` vbox

  -- show the widget and run the main loop
  windowSetDefaultSize win 600 600
  widgetShowAll win
  
  -- create two regions corresponding 
  putStrLn "creating mb1 region"
  mb1 <- regionCreateFrom c rootRegion magic1braced False
  putStrLn $ "creating ed1 subregion from mb1: " ++ show mb1
  ed1 <- regionCreate c mb1 (newPos fnm 1 2) True "..."
  putStrLn $ "created ed1 region " ++ (show ed1)
  mb2 <- regionCreateFrom c rootRegion magic2braced False
  ed2 <- regionCreateFrom c mb2 ((newPos fnm 1 2),(newPos fnm 1 5)) True 
  --putStrLn $ "create region  " ++ (show ed2)
  _ <- on b7 buttonActivated (moveIt c mb2)
  _ <- on b6 buttonActivated (rgnText c ed2)

  putStrLn "calling mainGUI"

  mainGUI

moveIt :: CwAPI -> CodeWidget.Region -> IO ()
moveIt c tr = do
    sel <- regionGetSelection c 
    case sel of 
          Nothing -> return ()
          Just sx -> do let r = selRegion sx
                        let f = selFrom   sx
                        let t = selTo     sx
                        putStrLn $ "moveIt: R: " ++ show r ++ " F:" ++ show f ++ " T:" ++ show t
                        regionMoveText c r tr (f,t)
                


rgnText :: CwAPI -> CodeWidget.Region -> IO ()
rgnText c r = do t <- regionGetText c r
                 dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk t
                 _ <- onResponse dialog (\_ -> widgetDestroy dialog)
                 windowPresent dialog
                 
  
