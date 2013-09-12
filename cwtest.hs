-- Test file for the SourceView widget.
module Main where

import Graphics.UI.Gtk
import CodeWidget
--import Text.Parsec
import Text.Parsec.Pos

{--
12{}24 Inside the braces is the 1st editable region, second is below. Set text used to insert ellipsis during runtime. 
--}

magicPos1 = newPos   "cwtest.hs" 10 4
hlite1_start = newPos "cwtest.hs" 18 7
hlite1_end = newPos   "cwtest.hs" 18 57

{--
12{}34This text should highlight when hilite1 is pressed
--}

  -- This text should highlight when hilite2 is pressed ... out to here
magicPos2 = newPos "cwtest.hs" 18 4
hlite2_start = newPos "cwtest.hs" 21 3
hlite2_end = newPos "cwtest.hs" 21 72

colorCont = "#80c080"

main = do
  _ <- initGUI
  win <- windowNew
  windowSetPosition win WinPosCenter
  _ <- win `onDestroy` mainQuit


  -- load up and display a file
  fileContents <- readFile "cwtest.hs"

  -- create a new CodeWidget
  cd <- codeWidgetNew "haskell" (Just "cwtest.hs") 600 600 
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
  widgetShow b1
  widgetShow b2
  widgetShow b3
  widgetShow b4
  widgetShow b5
  widgetShow b6
  containerAdd bbox b1
  containerAdd bbox b2
  containerAdd bbox b3
  containerAdd bbox b4
  containerAdd bbox b5
  containerAdd bbox b6
  _ <- on b1 buttonActivated ((regionApplyTag c) rootRegion htag1 (hlite1_start, hlite1_end))
  _ <- on b2 buttonActivated ((regionRemoveTag c) rootRegion htag1)
  _ <- on b4 buttonActivated ((regionApplyTag c) rootRegion htag2 (hlite2_start, hlite2_end))
  _ <- on b5 buttonActivated ((regionRemoveTag c) rootRegion htag2)
  _ <- on b3 buttonActivated (do s <- (regionGetText c) rootRegion
                                 putStrLn s)

  win `containerAdd` vbox

  -- show the widget and run the main loop
  windowSetDefaultSize win 600 600
  widgetShowAll win
  
  -- create two regions corresponding 
  ed1 <- regionCreate c rootRegion magicPos1 True "..."
  putStrLn $ "created region " ++ (show ed1)
  ed2 <- regionCreate c rootRegion magicPos2 True "..."
  putStrLn $ "create region  " ++ (show ed2)
  _ <- on b6 buttonActivated (do s <- (regionGetText c) ed2
                                 putStrLn s)

  putStrLn "calling mainGUI"

  mainGUI

