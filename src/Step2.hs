-- | Like Step1 but with Open / Save / Save As... support
module Step2 (step2) where

import Graphics.UI.WX
import Graphics.UI.WXCore

-- | We create a record for the context to simplify parameter passing in al event
--   handlers.  It seems pretty useless now, but you'll see this purpose later
data GUIContext = GUICtx { guiWin    :: Frame (),
                           guiEditor :: TextCtrl (),
                           guiFile   :: Var (Maybe FilePath) -- ^ The path of the current file
                           }

step2 :: IO ()
step2 =
    do
        win <- frame [text := "wxhNotepad - Step 2", visible := False]

        editor <- textCtrl win [font := fontFixed,
                                text := "Now the user can open a file, save it" ++
                                        "or save it with another name.\n" ++
                                        "Our program is smart enough to remember" ++
                                        "the path of the last opened/saved file\n" ++
                                        "Note that we're *not* catching any " ++
                                        "filesystem errors here.  That's left " ++
                                        "as a homework for you :P"]
        
        filePath <- varCreate Nothing

        -- We define the context to use it on every event handling function
        let guiCtx = GUICtx win editor filePath
        
        -- We create a menu for the window with the three items we want to add
        mnuFile <- menuPane [text := "File"]
        -- Just for fun, we use WXCore methods instead of WX ones
        menuAppend mnuFile wxID_OPEN "&Open...\tCtrl-o" "Open Page" False
        menuAppend mnuFile wxID_SAVE "&Save\tCtrl-s" "Save Page" False
        menuAppend mnuFile wxID_SAVEAS "Save &as...\tCtrl-Shift-s" "Save Page as" False
        menuAppend mnuFile wxID_CLOSE "&Close\tCtrl-W" "Close Page" False
        -- We associate the corresponding action to each menuItem
        -- the actions are defined below in this module
        evtHandlerOnMenuCommand win wxID_OPEN $ openPage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVE $ savePage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVEAS $ savePageAs guiCtx
        evtHandlerOnMenuCommand win wxID_CLOSE $ windowClose win False >> return ()
        -- And finally we add the bar to the window
        set win [menuBar := [mnuFile]]

        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        focusOn editor
        set win [visible := True]

savePageAs, savePage, openPage :: GUIContext -> IO ()
openPage GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        -- WXCore provides a couple of useful dialog methods, like this one.
        -- see: http://hackage.haskell.org/packages/archive/wxcore/latest/doc/html/Graphics-UI-WXCore-Dialogs.html#3
        maybePath <- fileOpenDialog win True True "Open file..." [("Haskells (*.hs)",["*.hs"]),
                                                                  ("Texts (*.txt)", ["*.txt"]),
                                                                  ("Any file (*.*)",["*.*"])] "" ""
        case maybePath of
            Nothing ->
                -- The user cancelled... nothing to do
                return ()
            Just path ->
                do
                    -- We put the text on the box
                    textCtrlLoadFile editor path
                    -- Usually, you want to see the name of the file in the window title
                    set win [text := "wxhnotepad - " ++ path]
                    -- and set the path in our variable
                    varSet filePath $ Just path

savePageAs GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        -- WXCore provides a couple of useful dialog methods, like this one.
        -- see: http://hackage.haskell.org/packages/archive/wxcore/latest/doc/html/Graphics-UI-WXCore-Dialogs.html#v%3AfileSaveDialog
        maybePath <- fileSaveDialog win True True "Save file..." [("Haskells (*.hs)",["*.hs"]),
                                                                  ("Texts (*.txt)", ["*.txt"]),
                                                                  ("Any file (*.*)",["*.*"])] "" ""
        case maybePath of
            Nothing ->
                -- The user cancelled... nothing to do
                return ()
            Just path ->
                do
                    -- We send the text to the file...
                    textCtrlSaveFile editor path
                    -- Usually, you want to see the name of the file in the window title
                    set win [text := "wxhnotepad - " ++ path]
                    -- and set the path in our variable
                    varSet filePath $ Just path

savePage guiCtx@GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        maybePath <- varGet filePath
        case maybePath of
            Nothing ->
                savePageAs guiCtx
            Just path ->
                -- We just send the text to the file...
                textCtrlSaveFile editor path >> return ()