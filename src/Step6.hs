-- | Like Step5 but with Toolbar, Statusbar and Context Menus
module Step6 (step6) where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (wxID_CUT, wxID_COPY, wxID_PASTE,
                                  wxID_FIND, wxID_FORWARD, wxID_REPLACE, wxID_BACKWARD)
import Data.Bits
import Data.Char (toLower)
import Data.List
import Paths_wxhnotepad -- That's the cabal way of finding the image paths

data FRFlags = FRFlags {frfGoingDown :: Bool,
                        frfMatchCase :: Bool,
                        frfWholeWord :: Bool,
                        frfWrapSearch :: Bool}
    deriving (Eq, Show)

data GUIContext = GUICtx { guiWin    :: Frame (),
                           guiEditor :: TextCtrl (),
                           guiFile   :: Var (Maybe FilePath),
                           guiTimer  :: TimerEx (),
                           guiPast   :: Var [String],
                           guiFuture :: Var [String],
                           guiSearch :: FindReplaceData (),
                           guiStatus :: StatusField -- ^ We'll use just one status field for now
                           }

wxID_MYUNDO, wxID_MYREDO, wxID_CUT, wxID_COPY, wxID_PASTE,
    wxID_FIND, wxID_FORWARD, wxID_BACKWARD, wxID_REPLACE :: Id
wxID_MYUNDO = 5108
wxID_MYREDO = 5109
wxID_CUT    = 5031
wxID_COPY   = 5032
wxID_PASTE  = 5033
wxID_FIND       = 5035   --HACK: They're not correctly numbered in WxcDefs or they
wxID_REPLACE    = 5038   --      don't even exist
wxID_FORWARD    = 5106
wxID_BACKWARD   = 5107

step6 :: IO ()
step6 =
    do
        win <- frame [text := "wxhNotepad - Step 6", visible := False]
        editor <- textCtrl win [font := fontFixed,
                                text := "Now we have a fully functional text " ++
                                        "editor, so let's decorate it a bit." ++
                                        "In this step we add toolbar, statusbar " ++
                                        "and context menues to our application.\n" ++
                                        "For the toolbar, we choose a particular " ++
                                        "set of images from SphericalIcons Set by " ++
                                        "Ahmad Hania (http://www.iconfinder.net/search/?q=iconset:sphericalcons) " ++
                                        "and we put those in the cabal package " ++
                                        "description.  We mention that because " ++
                                        "it's one thing to do besides this module.\n" ++
                                        "In this tutorial we'll use the statusbar " ++
                                        "just to show a couple of dummy messages, " ++
                                        "your homework is to add smart things to " ++
                                        "it, like a Line/Coulmn position marker."]
        filePath <- varCreate Nothing
        refreshTimer <- timer win []
        past <- varCreate []
        future <- varCreate []
        search <- findReplaceDataCreate wxFR_DOWN

        -- We create a status field where we'll put just text messages
        status <- statusField [text := "hello, this is wxhNotepad... happy editing :)"]
        -- The window statusbar is a list of status fields, we use just one
        set win [statusBar := [status]]
        
        let guiCtx = GUICtx win editor filePath refreshTimer past future search status
        
        set editor [on keyboard := \_ -> restartTimer guiCtx >> propagateEvent,
                    -- We associate the right click on the editor to a context menu launcher
                    on mouse :=  \e -> case e of
                                            MouseRightDown _ _ -> contextMenu guiCtx
                                            _ -> propagateEvent]

        timerOnCommand refreshTimer $ updatePast guiCtx
        updatePast guiCtx
        
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

        -- Once all menues are created, we create the window toolbar
        tbMain <- toolBarEx win True True []
        -- As we've created them using WXCore technics we have to find the items
        -- we want on the toolbar to have them in variables
        mitOpen <- menuFindItem mnuFile wxID_OPEN
        mitSave <- menuFindItem mnuFile wxID_SAVE
        mitSaveAs <- menuFindItem mnuFile wxID_SAVEAS
        mitClose <- menuFindItem mnuFile wxID_CLOSE
        mitCut <- menuFindItem mnuEdit wxID_CUT
        mitCopy <- menuFindItem mnuEdit wxID_COPY
        mitPaste <- menuFindItem mnuEdit wxID_PASTE
        mitFind <- menuFindItem mnuEdit wxID_FIND
        -- We get the paths of the images too
        openPath <- imageFile "open.png"
        savePath <- imageFile "save.png"
        saveAsPath <- imageFile "saveas.png"
        closePath <- imageFile "close.png"
        cutPath <- imageFile "cut.png"
        copyPath <- imageFile "copy.png"
        pastePath <- imageFile "paste.png"
        findPath <- imageFile "find.png"
        -- And finally we build our toolbar
        toolMenu tbMain mitOpen "Open" openPath [tooltip := "Open Document"]
        toolMenu tbMain mitSave "Save" savePath [tooltip := "Save Document"]
        toolMenu tbMain mitSaveAs "Save As..." saveAsPath [tooltip := "Save Document As..."]
        toolMenu tbMain mitClose "Close" closePath [tooltip := "Close Document"]
        toolBarAddSeparator tbMain
        toolMenu tbMain mitCut "Cut" cutPath [tooltip := "Cut"]
        toolMenu tbMain mitCopy "Copy" copyPath [tooltip := "Copy"]
        toolMenu tbMain mitPaste "Paste" pastePath [tooltip := "Paste"]
        toolBarAddSeparator tbMain
        toolMenu tbMain mitFind "Find..." findPath [tooltip := "Open Find Dialog"]
        toolBarSetToolBitmapSize tbMain $ sz 32 32

        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        focusOn editor
        set win [visible := True]

savePageAs, savePage, openPage,
    undo, redo, restartTimer, killTimer,
    updatePast, clearPast,
    cut, copy, paste,
    justFind, justFindNext, justFindPrev, findReplace,
    contextMenu :: GUIContext -> IO ()
openPage guiCtx@GUICtx{guiWin       = win,
                       guiEditor    = editor,
                       guiFile      = filePath,
                       guiStatus    = status} =
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
                    fileLoaded <- textCtrlLoadFile editor path
                    case fileLoaded of
                        True ->
                            set status [text := path ++ " loaded succesfully"]
                        False ->
                            set status [text := "Couldn't load " ++ path]
                    updatePast guiCtx
                    set win [text := "wxhnotepad - " ++ path]
                    varSet filePath $ Just path

savePageAs GUICtx{guiWin    = win,
                  guiEditor = editor,
                  guiFile   = filePath,
                  guiStatus = status} =
    do
        maybePath <- fileSaveDialog win True True "Save file..." [("Haskells (*.hs)",["*.hs"]),
                                                                  ("Texts (*.txt)", ["*.txt"]),
                                                                  ("Any file (*.*)",["*.*"])] "" ""
        case maybePath of
            Nothing ->
                return ()
            Just path ->
                do
                    fileSaved <- textCtrlSaveFile editor path
                    case fileSaved of
                        True ->
                            set status [text := path ++ " saved succesfully"]
                        False ->
                            set status [text := "Couldn't save " ++ path]
                    set win [text := "wxhnotepad - " ++ path]
                    varSet filePath $ Just path

savePage guiCtx@GUICtx{guiWin   = win,
                       guiEditor= editor,
                       guiFile  = filePath,
                       guiStatus= status} =
    do
        maybePath <- varGet filePath
        case maybePath of
            Nothing ->
                savePageAs guiCtx
            Just path ->
                do
                    fileSaved <- textCtrlSaveFile editor path
                    case fileSaved of
                        True ->
                            set status [text := path ++ " saved succesfully"]
                        False ->
                            set status [text := "Couldn't save " ++ path]
                
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

restartTimer guiCtx@GUICtx{guiWin = win, guiTimer = refreshTimer} =
    do
        started <- timerStart refreshTimer 1000 True
        if started
            then return ()
            else do
                    errorDialog win "Error" "Can't start more timers"
                    wxcAppExit

killTimer GUICtx{guiTimer = refreshTimer} = timerStop refreshTimer

copy GUICtx{guiEditor = editor} = textCtrlCopy editor

cut guiCtx@GUICtx{guiEditor = editor} = textCtrlCut editor >> updatePast guiCtx

paste guiCtx@GUICtx{guiEditor = editor} = textCtrlPaste editor >> updatePast guiCtx

justFind guiCtx = openFindDialog guiCtx "Find..." dialogDefaultStyle

justFindNext guiCtx@GUICtx{guiSearch = search} =
    do
        curFlags <- findReplaceDataGetFlags search
        findReplaceDataSetFlags search $ curFlags .|. wxFR_DOWN
        findNextButton guiCtx

justFindPrev guiCtx@GUICtx{guiSearch = search} =
    do
        curFlags <- findReplaceDataGetFlags search
        findReplaceDataSetFlags search $ curFlags .&. complement wxFR_DOWN
        findNextButton guiCtx

findReplace guiCtx = openFindDialog guiCtx "Find and Replace..." $ dialogDefaultStyle .|. wxFR_REPLACEDIALOG

buildFRFlags :: Bool -> Int -> IO FRFlags
buildFRFlags w x = return FRFlags {frfGoingDown = (x .&. wxFR_DOWN) /= 0,
                                   frfMatchCase = (x .&. wxFR_MATCHCASE) /= 0,
                                   frfWholeWord = (x .&. wxFR_WHOLEWORD) /= 0,
                                   frfWrapSearch = w}

openFindDialog :: GUIContext -> String -> Int -> IO ()
openFindDialog guiCtx@GUICtx{guiWin = win,
                             guiSearch = search} title dlgStyle =
    do
        frdialog <- findReplaceDialogCreate win search title $ dlgStyle + wxFR_NOWHOLEWORD
        let winSet k f = let hnd _ = f guiCtx >> propagateEvent
                          in windowOnEvent frdialog [k] hnd hnd
        winSet wxEVT_COMMAND_FIND findNextButton
        winSet wxEVT_COMMAND_FIND_NEXT findNextButton
        winSet wxEVT_COMMAND_FIND_REPLACE findReplaceButton
        winSet wxEVT_COMMAND_FIND_REPLACE_ALL findReplaceAllButton
        set frdialog [visible := True]

findNextButton, findReplaceButton, findReplaceAllButton :: GUIContext -> IO ()
findNextButton guiCtx@GUICtx{guiEditor= editor,
                             guiWin   = win,
                             guiSearch= search} =
    do
        s <- findReplaceDataGetFindString search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        mip <- findMatch s fs editor
        case mip of
            Nothing ->
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip ->
                do
                    textCtrlSetInsertionPoint editor ip
                    textCtrlSetSelection editor ip (length s + ip)
                

findReplaceButton guiCtx@GUICtx{guiEditor   = editor,
                                guiWin      = win,
                                guiSearch   = search} =
    do
        s <- findReplaceDataGetFindString search
        r <- findReplaceDataGetReplaceString search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        mip <- findMatch s fs editor
        case mip of
            Nothing ->
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip ->
                do
                    textCtrlReplace editor ip (length s + ip) r
                    textCtrlSetInsertionPoint editor ip
                    textCtrlSetSelection editor ip (length r + ip)
                    updatePast guiCtx
        
findReplaceAllButton guiCtx@GUICtx{guiEditor = editor,
                                   guiSearch = search} =
    do
        s <- findReplaceDataGetFindString search
        r <- findReplaceDataGetReplaceString search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags False
        textCtrlSetInsertionPoint editor 0
        replaceAllIn s r fs editor
        updatePast guiCtx
    where replaceAllIn s r fs editor =
            do
                mip <- findMatch s fs editor
                case mip of
                    Nothing ->
                        return ()
                    Just ip ->
                        do
                            textCtrlReplace editor ip (length s + ip) r
                            textCtrlSetInsertionPoint editor $ length r + ip
                            replaceAllIn s r fs editor

findMatch :: String -> FRFlags -> TextCtrl () -> IO (Maybe Int)
findMatch query flags editor =
    do
        txt <- get editor text
        ip <- textCtrlGetInsertionPoint editor
        let (substring, string) = if frfMatchCase flags
                                    then (query, txt)
                                    else (map toLower query, map toLower txt)
            funct = if frfGoingDown flags
                        then nextMatch (ip + 1)
                        else prevMatch ip
            (mip, wrapped) = funct substring string
        return $ if (not $ frfWrapSearch flags) && wrapped
                    then Nothing
                    else mip

prevMatch, nextMatch :: Int -> String -> String -> (Maybe Int, Bool)
prevMatch _ [] _ = (Nothing, True)
prevMatch from substring string | length string < from || from <= 0 = prevMatch (length string) substring string
                                | otherwise =
                                        case nextMatch (fromBack from) (reverse substring) (reverse string) of
                                            (Nothing, wrapped) -> (Nothing, wrapped)
                                            (Just ri, wrapped) -> (Just $ fromBack (ri + length substring), wrapped)
    where fromBack x = length string - x

nextMatch _ [] _ = (Nothing, True)
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

contextMenu guiCtx@GUICtx{guiWin = win, guiEditor = editor} =
    do
        -- We create the context menu, which is just a menuPane
        contextMenu <- menuPane []
        -- We'll present cut/copy items only if there's some text selected
        sel <- textCtrlGetStringSelection editor
        case sel of
            "" -> -- If nothing is selected we let the event propagate
                return ()
            _ ->
                do
                    menuAppend contextMenu wxID_CUT "C&ut\tCtrl-x" "Cut" False
                    menuAppend contextMenu wxID_COPY "&Copy\tCtrl-c" "Copy" False
        -- We always add the paste item
        menuAppend contextMenu wxID_PASTE "&Paste\tCtrl-v" "Paste" False
        -- We propagate the event so the menu presentation is the last thing to happen
        propagateEvent
        -- We detect the position in the screen where the mouse is pointing
        pointWithinWindow <- windowGetMousePosition win
        -- And we make the menu pop up
        menuPopup contextMenu pointWithinWindow win
        -- Finally, we delete the menu
        objectDelete contextMenu

-- | This function takes a name and, with a little knowlegde and the help of
--   cabal, returns the path of that image
imageFile :: String -> IO FilePath
imageFile img = getDataFileName $ "res/images/" ++ img
