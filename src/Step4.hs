-- | Like Step3 but with Cut / Copy / Paste support
module Step4 (step4) where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (wxID_CUT, wxID_COPY, wxID_PASTE)

data GUIContext = GUICtx { guiWin    :: Frame (),
                           guiEditor :: TextCtrl (),
                           guiFile   :: Var (Maybe FilePath),
                           guiTimer  :: Var (TimerEx ()),
                           guiPast   :: Var [String],
                           guiFuture :: Var [String]
                           }

wxID_MYUNDO, wxID_MYREDO, wxID_CUT, wxID_COPY, wxID_PASTE :: Id
wxID_MYUNDO = 5107
wxID_MYREDO = 5108
wxID_CUT    = 5031 --HACK: They're not correctly numbered in WxcDefs
wxID_COPY   = 5032
wxID_PASTE  = 5033

step4 :: IO ()
step4 =
    do
        win <- frame [text := "wxhNotepad - Step 4", visible := False]
        editor <- textCtrl win [font := fontFixed,
                                text := "Again, Cut/Copy/Past support comes with " ++
                                        "text controls, but we want to have " ++
                                        "a menu item for it too.\n" ++
                                        "Besides, we need to adapt our Undo/Redo " ++
                                        "implementation to these functions.\n"]
        filePath <- varCreate Nothing
        refreshTimer <- timer win [interval   := 1000000,
                                   on command := return ()]
        varTimer <- varCreate refreshTimer
        past <- varCreate []
        future <- varCreate []
        let guiCtx = GUICtx win editor filePath varTimer past future
        
        set editor [on keyboard := \_ -> restartTimer guiCtx >> propagateEvent]
        updatePast guiCtx
        
        -- We create a menu for the window with the same items from steps 2 & 3
        -- and a new items in the edit menu
        mnuFile <- menuPane [text := "File"]
        mnuEdit <- menuPane [text := "Edit"]
        menuAppend mnuFile wxID_OPEN "&Open...\tCtrl-o" "Open Page" False
        menuAppend mnuFile wxID_SAVE "&Save\tCtrl-s" "Save Page" False
        menuAppend mnuFile wxID_SAVEAS "Save &as...\tCtrl-Shift-s" "Save Page as" False
        menuAppend mnuFile wxID_CLOSE "&Close\tCtrl-W" "Close Page" False
        menuAppend mnuEdit wxID_MYUNDO "&Undo\tCtrl-z" "Undo last action" False
        menuAppend mnuEdit wxID_MYREDO "&Redo\tCtrl-Shift-z" "Redo last undone action" False
        menuAppendSeparator mnuEdit
        menuAppend mnuEdit wxID_CUT "C&ut\tCtrl-x" "Cut" False
        menuAppend mnuEdit wxID_COPY "&Copy\tCtrl-c" "Copy" False
        menuAppend mnuEdit wxID_PASTE "&Paste\tCtrl-v" "Paste" False

        evtHandlerOnMenuCommand win wxID_OPEN $ openPage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVE $ savePage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVEAS $ savePageAs guiCtx
        evtHandlerOnMenuCommand win wxID_CLOSE $ windowClose win False >> return ()
        evtHandlerOnMenuCommand win wxID_MYUNDO $ undo guiCtx
        evtHandlerOnMenuCommand win wxID_MYREDO $ redo guiCtx
        evtHandlerOnMenuCommand win wxID_CUT $ cut guiCtx
        evtHandlerOnMenuCommand win wxID_COPY $ copy guiCtx
        evtHandlerOnMenuCommand win wxID_PASTE $ paste guiCtx
        set win [menuBar := [mnuFile, mnuEdit]]

        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        focusOn editor
        set win [visible := True]

savePageAs, savePage, openPage,
    undo, redo, restartTimer, killTimer,
    updatePast, clearPast,
    cut, copy, paste :: GUIContext -> IO ()
openPage guiCtx@GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        maybePath <- fileOpenDialog win True True "Open file..." [("Haskells (*.hs)",["*.hs"]),
                                                                  ("Texts (*.txt)", ["*.txt"]),
                                                                  ("Any file (*.*)",["*.*"])] "" ""
        case maybePath of
            Nothing ->
                return ()
            Just path ->
                do
                    clearPast guiCtx
                    textCtrlLoadFile editor path
                    updatePast guiCtx
                    set win [text := "wxhnotepad - " ++ path]
                    varSet filePath $ Just path

savePageAs GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        maybePath <- fileSaveDialog win True True "Save file..." [("Haskells (*.hs)",["*.hs"]),
                                                                  ("Texts (*.txt)", ["*.txt"]),
                                                                  ("Any file (*.*)",["*.*"])] "" ""
        case maybePath of
            Nothing ->
                return ()
            Just path ->
                do
                    textCtrlSaveFile editor path
                    set win [text := "wxhnotepad - " ++ path]
                    varSet filePath $ Just path

savePage guiCtx@GUICtx{guiWin = win, guiEditor = editor, guiFile = filePath} =
    do
        maybePath <- varGet filePath
        case maybePath of
            Nothing ->
                savePageAs guiCtx
            Just path ->
                textCtrlSaveFile editor path >> return ()
                
undo guiCtx@GUICtx{guiEditor = editor, guiPast = past, guiFuture = future} =
    do
        updatePast guiCtx
        history <- varGet past
        case history of
            [] ->
                return ()
            [t] ->
                return ()
            tnow:tlast:ts ->
                do
                    varUpdate future (tnow:)
                    varSet past $ tlast:ts
                    set editor [text := tlast]                                        
                    
redo guiCtx@GUICtx{guiEditor = editor, guiPast = past, guiFuture = future} =
    do
        updatePast guiCtx
        coming <- varGet future
        case coming of
            [] ->
                return ()
            t:ts ->
                do
                    varSet future ts
                    varUpdate past (t:)
                    set editor [text := t]

updatePast guiCtx@GUICtx{guiEditor = editor, guiPast = past, guiFuture = future} =
    do
        tnow <- get editor text
        history <- varGet past
        case history of
            [] ->
                varSet past [tnow]
            t:_ ->
                if t /= tnow
                    then do
                        varUpdate past (tnow:)
                        varSet future []
                    else
                        return ()
        killTimer guiCtx

clearPast GUICtx{guiPast = past, guiFuture = future} =
    do
        varSet past []
        varSet future []

restartTimer guiCtx@GUICtx{guiWin = win, guiTimer = varTimer} =
    do
        newRefreshTimer <- timer win [interval := 1000,
                                      on command := updatePast guiCtx]
        refreshTimer <- varSwap varTimer newRefreshTimer
        timerOnCommand refreshTimer $ return ()

killTimer GUICtx{guiWin = win, guiTimer = varTimer} =
    do
        newRefreshTimer <- timer win [interval := 1000000,
                                      on command := return ()]
        refreshTimer <- varSwap varTimer newRefreshTimer
        timerOnCommand refreshTimer $ return ()

-- We just copy the selected text
copy GUICtx{guiEditor = editor} = textCtrlCopy editor

-- We cut the selected text and, as the editor is modified, we update the history
cut guiCtx@GUICtx{guiEditor = editor} = textCtrlCut editor >> updatePast guiCtx

-- We paste the clipboard contents on the editor and, as it's modified, we update the history
paste guiCtx@GUICtx{guiEditor = editor} = textCtrlPaste editor >> updatePast guiCtx