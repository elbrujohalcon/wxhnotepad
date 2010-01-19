-- | This module provides the application main "window" which is no real window
--   at all.  It just holds the menus so you can open each step of the tutorial
--   or close it all at the same time.
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Step1
import Step2

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
        menuItem mnuSteps [text := "Step &4\tCtrl-3", on command := say "Step 3" "Undo / Redo"]
        menuItem mnuSteps [text := "Step &3\tCtrl-4", on command := say "Step 4" "Cut / Copy / Paste"]
        menuItem mnuSteps [text := "Step &5\tCtrl-5", on command := say "Step 5" "Toolbar / Statusbar / Context Menus"]
        menuItem mnuSteps [text := "Step &6\tCtrl-6", on command := say "Step 6" "Preferences..."]
        menuQuit mnuSteps [on command := wxcAppExit]

        -- the simplest about page...
        mnuHelp <- menuHelp []
        menuAbout mnuHelp [on command := say "About wxHNotepad" "Author: Fernando Brujo Benavides\nWebsite: http://github.com/elbrujohalcon/wxhnotepad"]
        
        -- and... RUN, FORREST!!
        set win [menuBar := [mnuSteps, mnuHelp],
                 visible := True, clientSize := sz 0 0]
                 --HACK: Linux and Windows do not support invisible windows with menuBars on them
                 --      Then, we need to "show" a really small window instead.