-- | Like Step2 but with Undo / Redo... support
module Step3 (step3) where

import Graphics.UI.WX
import Graphics.UI.WXCore

data GUIContext = GUICtx { guiWin    :: Frame (),
                           guiEditor :: TextCtrl (),
                           guiFile   :: Var (Maybe FilePath),
                           guiTimer  :: TimerEx (),       -- ^ A timer to detect user actions
                           guiPast   :: Var [String],     -- ^ For Undo history
                           guiFuture :: Var [String]      -- ^ For Redo history
                           }

wxID_MYUNDO, wxID_MYREDO :: Id --HACK: because we're using them in a different way
wxID_MYUNDO = 5107
wxID_MYREDO = 5108

step3 :: IO ()
step3 =
    do
        win <- frame [text := "wxhNotepad - Step 3", visible := False]
        editor <- textCtrl win [font := fontFixed,
                                text := "Undo / Redo support is ready by default " ++
                                        "on text controls, but we want to have " ++
                                        "a menu item for it too.\n" ++
                                        "Another important thing to notice is " ++
                                        "that you don't want to let the user 'undo' " ++
                                        "anything when he opens a new file. That's " ++
                                        "why we'll not use the default textCtrl " ++
                                        "functions and we'll develop our own ones.\n" ++
                                        "That means a lot of code for a seemingly " ++
                                        "small functionallity, but it will let " ++
                                        "you learn about timers and history too :)\n" ++
                                        "What we do here fails to exactly emulate " ++
                                        "what we're used to see in text editors " ++
                                        "as undo/redo behaviour because it's " ++
                                        "not considering insertion-point changes.  " ++
                                        "As you might be guessing now, that's not " ++
                                        "difficult to implement, so it's left as " ++
                                        "a homework :)"]
        filePath <- varCreate Nothing

        -- We create a timer to detect user actions.  This way we'll not undo/redo
        -- every character
        refreshTimer <- timer win []
        
        -- We define the context to use it on every event handling function
        past <- varCreate []
        future <- varCreate []
        let guiCtx = GUICtx win editor filePath refreshTimer past future
        
        -- We set the command associated to the timer (but we didn't start it yet)
        timerOnCommand refreshTimer $ updatePast guiCtx 
        
        -- We associate the events on the editor with commands that populate undo/redo history
        set editor [on keyboard := \_ -> restartTimer guiCtx >> propagateEvent]
        -- and we start populating the history because there's text on the editor...
        updatePast guiCtx
        
        -- We create a menu for the window with the same items from step 2
        -- and a new one for the edition controls
        mnuFile <- menuPane [text := "File"]
        mnuEdit <- menuPane [text := "Edit"]
        menuAppend mnuFile wxID_OPEN "&Open...\tCtrl-o" "Open Page" False
        menuAppend mnuFile wxID_SAVE "&Save\tCtrl-s" "Save Page" False
        menuAppend mnuFile wxID_SAVEAS "Save &as...\tCtrl-Shift-s" "Save Page as" False
        menuAppend mnuFile wxID_CLOSE "&Close\tCtrl-W" "Close Page" False
        menuAppend mnuEdit wxID_MYUNDO "&Undo\tCtrl-z" "Undo last action" False
        menuAppend mnuEdit wxID_MYREDO "&Redo\tCtrl-Shift-z" "Redo last undone action" False
        evtHandlerOnMenuCommand win wxID_OPEN $ openPage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVE $ savePage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVEAS $ savePageAs guiCtx
        evtHandlerOnMenuCommand win wxID_CLOSE $ windowClose win False >> return ()
        evtHandlerOnMenuCommand win wxID_MYUNDO $ undo guiCtx
        evtHandlerOnMenuCommand win wxID_MYREDO $ redo guiCtx
        set win [menuBar := [mnuFile, mnuEdit]]

        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        focusOn editor
        set win [visible := True]

savePageAs, savePage, openPage,
    undo, redo, restartTimer, killTimer,
    updatePast, clearPast :: GUIContext -> IO ()
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
                    -- We clear the undo history
                    clearPast guiCtx
                    textCtrlLoadFile editor path
                    -- Now we set the first action in undo history with the current text
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
        -- Now we try to detect if there's something new and kill the timer
        updatePast guiCtx
        history <- varGet past
        case history of
            [] ->
                return () -- Nothing to be undone
            [t] ->
                return () -- Just the initial state, nothing to be undone
            tnow:tlast:ts ->
                do
                    -- We add an action to be done on redo
                    varUpdate future (tnow:)
                    -- we remove it from the history
                    varSet past $ tlast:ts
                    -- and set the editor accordingly
                    set editor [text := tlast]                                        
                    
redo guiCtx@GUICtx{guiEditor = editor, guiPast = past, guiFuture = future} =
    do
        -- First of all, we try to detect if there's something new
        updatePast guiCtx
        coming <- varGet future
        case coming of
            [] ->
                return () -- Nothing to be redone
            t:ts ->
                do
                    -- remove it from the coming actions
                    varSet future ts
                    -- move it to the history actions
                    varUpdate past (t:)
                    -- and set the editor accordingly
                    set editor [text := t]

updatePast guiCtx@GUICtx{guiEditor = editor, guiPast = past, guiFuture = future} =
    do
        -- We take the current history and add a new first event
        tnow <- get editor text
        history <- varGet past
        case history of
            [] ->
                varSet past [tnow]
            t:_ ->
                if t /= tnow -- if there was a real change, we recorded it
                    then do
                        varUpdate past (tnow:)
                        varSet future [] -- we clean the future because the user is writing a new one
                    else     -- otherwise we ignore it
                        return ()
        -- Now that history is up to date, we let the timer rest until new activity
        -- is detected
        killTimer guiCtx

clearPast GUICtx{guiPast = past, guiFuture = future} =
    do
        -- We start all over again...
        varSet past []
        varSet future []

restartTimer guiCtx@GUICtx{guiWin = win, guiTimer = refreshTimer} =
        -- The user did something, we start our timer with a 1sec interval and
        -- a one-shot type so it will run just once and then it will modify history
        -- if the user doesn't do anything in that sec.  If the user does another
        -- thing, then the timer will be restarted again and so on so far until
        -- the first sec of inactivity
    do
        started <- timerStart refreshTimer 1000 True
        if started
            then return ()
            else do
                    errorDialog win "Error" "Can't start more timers"
                    wxcAppExit

-- We kill the timer till there's new notices
killTimer GUICtx{guiTimer = refreshTimer} = timerStop refreshTimer