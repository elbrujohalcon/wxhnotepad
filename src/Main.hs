-- | This module provides the application main "window" which is no real window
--   at all.  It just holds the menus so you can open each step of the tutorial
--   or close it all at the same time.
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Step1
import Step2
import Step3
import Step4
import Step5

-- | The application entry point
main :: IO ()
main = start gui

gui :: IO ()
gui =
    do
        -- We create a hidden window
        win <- frame [text := "wxhNotepad", visible := False]

        -- If this window is closed, the application ends
        set win [on closing := wxcAppExit]
        
        let say title desc = infoDialog win title desc
        
        -- The only thing that this window will have is its menu bar to open all
        -- the other windows
        mnuSteps <- menuPane [text := "Steps"]
        menuItem mnuSteps [on command := step1, text := "Step &1 - Just a Text Field\tCtrl-1"]
        menuItem mnuSteps [on command := step2, text := "Step &2 - Open / Save / Save As...\tCtrl-2"]
        menuItem mnuSteps [on command := step3, text := "Step &3 - Undo / Redo...\tCtrl-3"]
        menuItem mnuSteps [on command := step4, text := "Step &4 - Cut / Copy / Paste...\tCtrl-4"]
        menuItem mnuSteps [on command := step5, text := "Step &5 - Find / Replace...\tCtrl-5"]
        menuItem mnuSteps [text := "Step &5\tCtrl-6", on command := say "Step 6" "Toolbar / Statusbar / Context Menus"]
        menuItem mnuSteps [text := "Step &6\tCtrl-7", on command := say "Step 7" "Preferences..."]
        menuQuit mnuSteps [on command := wxcAppExit]

        -- the simplest about page...
        mnuHelp <- menuHelp []
        menuAbout mnuHelp [on command := say "About wxHNotepad" "Author: Fernando Brujo Benavides\nWebsite: http://github.com/elbrujohalcon/wxhnotepad"]
        
        -- and... RUN, FORREST!!
        set win [menuBar := [mnuSteps, mnuHelp],
                 visible := True, clientSize := sz 0 0]
                 --HACK: Linux and Windows do not support invisible windows with menuBars on them
                 --      Then, we need to "show" a really small window instead.