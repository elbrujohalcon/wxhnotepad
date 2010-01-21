-- | Like Step4 but with Find / Replace support
module Step5 (step5) where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (wxID_CUT, wxID_COPY, wxID_PASTE,
                                  wxID_FIND, wxID_FORWARD, wxID_BACKWARD)
import Data.Bits
import Data.Char (toLower)
import Data.List

-- | /FRFlags/ represents what the user choose in the FindReplaceDialog
data FRFlags = FRFlags {frfGoingDown :: Bool,
                        frfMatchCase :: Bool,
                        frfWholeWord :: Bool,
                        frfWrapSearch :: Bool}
    deriving (Eq, Show)

data GUIContext = GUICtx { guiWin    :: Frame (),
                           guiEditor :: TextCtrl (),
                           guiFile   :: Var (Maybe FilePath),
                           guiTimer  :: Var (TimerEx ()),
                           guiPast   :: Var [String],
                           guiFuture :: Var [String],
                           guiSearch :: FindReplaceData () -- ^ Needed to hold the what the user is looking for
                           }

wxID_MYUNDO, wxID_MYREDO, wxID_CUT, wxID_COPY, wxID_PASTE,
    wxID_FIND, wxID_FORWARD, wxID_BACKWARD, wxID_REPLACE :: Id
wxID_MYUNDO = 5108
wxID_MYREDO = 5109
wxID_CUT    = 5031
wxID_COPY   = 5032
wxID_PASTE  = 5033
wxID_FIND       = 5035  --HACK: They're not correctly numbered in WxcDefs or they
wxID_REPLACE    = 5038  --      don't even exist
wxID_FORWARD    = 5106
wxID_BACKWARD   = 5107

step5 :: IO ()
step5 =
    do
        win <- frame [text := "wxhNotepad - Step 5", visible := False]
        editor <- textCtrl win [font := fontFixed,
                                text := "Find / Replace functionality is supported " ++
                                        "by wxHaskell but it's kinda hidden in " ++
                                        "WXCore.  We'll need a little digging for " ++
                                        "this step.\n" ++
                                        "This step involved tons of code, so I " ++
                                        "think there must be a better way to do it." ++
                                        "If you find it, please post it on the " ++
                                        "wxhaskell-users mailing list :)"]
        filePath <- varCreate Nothing
        refreshTimer <- timer win [interval   := 1000000,
                                   on command := return ()]
        varTimer <- varCreate refreshTimer
        past <- varCreate []
        future <- varCreate []
        -- We create a FindReplaceData that will hold the information about the
        -- last search
        search <- findReplaceDataCreate wxFR_DOWN
        let guiCtx = GUICtx win editor filePath varTimer past future search
        
        set editor [on keyboard := \_ -> restartTimer guiCtx >> propagateEvent]
        updatePast guiCtx
        
        -- We create a menu for the window with the same items from previous steps
        -- and a couple of new items in the edit menu
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
        menuAppendSeparator mnuEdit
        menuAppend mnuEdit wxID_FIND "&Find...\tCtrl-f" "Find" False
        menuAppend mnuEdit wxID_FORWARD "Find &Next\tCtrl-g" "Find Next" False
        menuAppend mnuEdit wxID_BACKWARD "Find &Previous\tCtrl-Shift-g" "Find Previous" False
        menuAppend mnuEdit wxID_REPLACE "&Replace...\tCtrl-Shift-r" "Replace" False

        evtHandlerOnMenuCommand win wxID_OPEN $ openPage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVE $ savePage guiCtx
        evtHandlerOnMenuCommand win wxID_SAVEAS $ savePageAs guiCtx
        evtHandlerOnMenuCommand win wxID_CLOSE $ windowClose win False >> return ()
        evtHandlerOnMenuCommand win wxID_MYUNDO $ undo guiCtx
        evtHandlerOnMenuCommand win wxID_MYREDO $ redo guiCtx
        evtHandlerOnMenuCommand win wxID_CUT $ cut guiCtx
        evtHandlerOnMenuCommand win wxID_COPY $ copy guiCtx
        evtHandlerOnMenuCommand win wxID_PASTE $ paste guiCtx
        evtHandlerOnMenuCommand win wxID_FIND $ justFind guiCtx
        evtHandlerOnMenuCommand win wxID_FORWARD $ justFindNext guiCtx
        evtHandlerOnMenuCommand win wxID_BACKWARD $ justFindPrev guiCtx
        evtHandlerOnMenuCommand win wxID_REPLACE $ findReplace guiCtx
        set win [menuBar := [mnuFile, mnuEdit]]

        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        focusOn editor
        set win [visible := True]

savePageAs, savePage, openPage,
    undo, redo, restartTimer, killTimer,
    updatePast, clearPast,
    cut, copy, paste,
    justFind, justFindNext, justFindPrev, findReplace :: GUIContext -> IO ()
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

copy GUICtx{guiEditor = editor} = textCtrlCopy editor

cut guiCtx@GUICtx{guiEditor = editor} = textCtrlCut editor >> updatePast guiCtx

paste guiCtx@GUICtx{guiEditor = editor} = textCtrlPaste editor >> updatePast guiCtx

-- We open a FindReplaceDialog with default style to let the user choose what to do
justFind guiCtx = openFindDialog guiCtx "Find..." dialogDefaultStyle

justFindNext guiCtx@GUICtx{guiSearch = search} =
    do
        -- We get the current search parameters
        curFlags <- findReplaceDataGetFlags search
        -- We set the finding direction to down
        findReplaceDataSetFlags search $ curFlags .|. wxFR_DOWN
        -- and we proceed with the search
        findNextButton guiCtx

justFindPrev guiCtx@GUICtx{guiSearch = search} =
    do
        -- We get the current search parameters
        curFlags <- findReplaceDataGetFlags search
        -- We set the finding direction to down
        findReplaceDataSetFlags search $ curFlags .&. complement wxFR_DOWN
        -- and we proceed with the search
        findNextButton guiCtx

-- We open a FindReplaceDialog with replace style
findReplace guiCtx = openFindDialog guiCtx "Find and Replace..." $ dialogDefaultStyle .|. wxFR_REPLACEDIALOG

-- | Auxiliary function to build a /FRFlags/
buildFRFlags :: Bool    -- ^ Wrap the Search?
                -> Int  -- ^ BitMask for Direction, Match Case and Whole Word flags
                -> IO FRFlags
buildFRFlags w x = return FRFlags {frfGoingDown = (x .&. wxFR_DOWN) /= 0,
                                   frfMatchCase = (x .&. wxFR_MATCHCASE) /= 0,
                                   frfWholeWord = (x .&. wxFR_WHOLEWORD) /= 0,
                                   frfWrapSearch = w}

-- | Opens a FindReplace Dialog
openFindDialog :: GUIContext    -- ^ The current GUIContext
                  -> String     -- ^ The title of the dialog
                  -> Int        -- ^ The style of the dialog
                  -> IO ()
openFindDialog guiCtx@GUICtx{guiWin = win,
                             guiSearch = search} title dlgStyle =
    do
        -- First we must create a dialog with the search parameters that we
        -- already have.  The dialog itself is going to modify them according
        -- to the user selections
        frdialog <- findReplaceDialogCreate win search title $ dlgStyle + wxFR_NOWHOLEWORD
        -- One of the weirdest functions on wxHaskell is windowOnEvent.
        -- I did not really understand what are the parameters for exactly, but
        -- if we use it this way, we manage to get a certain event with id k to
        -- fire the function f... :)
        let winSet k f = let hnd _ = f guiCtx >> propagateEvent
                          in windowOnEvent frdialog [k] hnd hnd
        -- Using that magic trick, we associate our functions with the button
        -- pressing events in the dialog...
        winSet wxEVT_COMMAND_FIND findNextButton
        winSet wxEVT_COMMAND_FIND_NEXT findNextButton
        winSet wxEVT_COMMAND_FIND_REPLACE findReplaceButton
        winSet wxEVT_COMMAND_FIND_REPLACE_ALL findReplaceAllButton
        -- And... it's showtime!!
        set frdialog [visible := True]

-- These 3 functions handle the button events in the dialog but also handle the
-- menuitems when the dialog is not there
findNextButton, findReplaceButton, findReplaceAllButton :: GUIContext -> IO ()
findNextButton guiCtx@GUICtx{guiEditor= editor,
                             guiWin   = win,
                             guiSearch= search} =
    do
        -- We check what the user is trying to find
        s <- findReplaceDataGetFindString search
        -- We parse it, assuming that the user wants to wrap its search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        -- We try to find a match in the text
        mip <- findMatch s fs editor
        case mip of
            Nothing -> -- If there's no match, we inform that to the user
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip -> -- If there's a match, we select that text
                do
                    textCtrlSetInsertionPoint editor ip
                    textCtrlSetSelection editor (length s + ip) ip
                

findReplaceButton guiCtx@GUICtx{guiEditor   = editor,
                                guiWin      = win,
                                guiSearch   = search} =
    do
        -- We check what the user is trying to find
        s <- findReplaceDataGetFindString search
        -- and what is he wanting to replace it with
        r <- findReplaceDataGetReplaceString search
        -- We parse it, assuming that the user wants to wrap its search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        -- We try to find a match in the text
        mip <- findMatch s fs editor
        case mip of
            Nothing -> -- If there's no match, we inform that to the user
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip ->
                do -- If there's a match, we replace that text
                    textCtrlReplace editor ip (length s + ip) r
                    -- select the result
                    textCtrlSetInsertionPoint editor ip
                    textCtrlSetSelection editor (length r + ip) ip
                    -- and finally update the history
                    updatePast guiCtx
        
findReplaceAllButton guiCtx@GUICtx{guiEditor = editor,
                                   guiSearch = search} =
    do
        -- We check what the user is trying to find
        s <- findReplaceDataGetFindString search
        -- and what is he wanting to replace it with
        r <- findReplaceDataGetReplaceString search
        -- We parse it, assuming that the user wants to wrap its search
        -- Note that we're NOT wrapping our search, to avoid infinite looping
        fs <- findReplaceDataGetFlags search >>= buildFRFlags False
        -- We start at the beginning of the text
        textCtrlSetInsertionPoint editor 0
        -- And we go through the text replacing s by r until there's nothing
        --  more to replace
        replaceAllIn s r fs editor
        -- and finally update the history
        updatePast guiCtx
    where replaceAllIn s r fs editor =
            do
                mip <- findMatch s fs editor
                case mip of
                    Nothing ->
                        return () -- we're done here
                    Just ip ->
                        do
                            textCtrlReplace editor ip (length s + ip) r
                            textCtrlSetInsertionPoint editor $ length r + ip
                            replaceAllIn s r fs editor -- we look for the next match

-- | Tries to find a string in a text control
findMatch :: String -- ^ The string to find
             -> FRFlags -- ^ The flags to know how to look for it
             -> TextCtrl () -- ^ The textControl
             -> IO (Maybe Int) -- ^ Nothing or Just the position of the first match
findMatch query flags editor =
    do
        -- We get the current text
        txt <- get editor text
        -- and the insertion point (that's where the search begins)
        ip <- textCtrlGetInsertionPoint editor
        -- If we're not required to match the case we move everything to lower
        let (substring, string) = if frfMatchCase flags
                                    then (query, txt)
                                    else (map toLower query, map toLower txt)
            -- we choose what function to use depending on the direction
            funct = if frfGoingDown flags
                        then nextMatch (ip + 1)
                        else prevMatch ip
            (mip, wrapped) = funct substring string
        -- if it had to wrap around and that was 'forbbiden', then the match didn't happen
        -- otherwise, the result is valid
        putStrLn . show $ ("findMatch", query, flags, txt, ip, mip, wrapped)
        return $ if (not $ frfWrapSearch flags) && wrapped
                    then Nothing
                    else mip

-- These functions try to find a string contained in another
prevMatch, nextMatch :: Int -- ^ Starting point
                        -> String -- ^ What to find
                        -> String -- ^ Where to find it
                        -> (Maybe Int, Bool) -- ^ (Nothing or Just the point where it was found, It needed to wrap around?)
prevMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
prevMatch from substring string | length string < from || from <= 0 = prevMatch (length string) substring string
                                | otherwise =
                                        case nextMatch (fromBack from) (reverse substring) (reverse string) of
                                            (Nothing, wrapped) -> (Nothing, wrapped)
                                            (Just ri, wrapped) -> (Just $ fromBack (ri + length substring), wrapped)
    where fromBack x = length string - x

nextMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
nextMatch from substring string | length substring > length string = (Nothing, True)
                                | length string <= from = nextMatch 0 substring string
                                | otherwise =
                                        let after = drop from string
                                            before = take (from + length substring) string
                                            aIndex = indexOf substring after
                                            bIndex = indexOf substring before
                                         in case aIndex of
                                                Just ai ->
                                                    (Just $ from + ai,  False)
                                                Nothing ->
                                                    case bIndex of
                                                        Nothing -> (Nothing, True)
                                                        Just bi -> (Just bi, True)
    
indexOf :: String -> String -> Maybe Int
indexOf substring string = findIndex (isPrefixOf substring) $ tails string