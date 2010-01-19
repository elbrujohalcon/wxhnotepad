-- | This module provides the simplest version of our text editor: just a big
--   textbox :)
module Step1 (step1) where

import Graphics.UI.WX
import Graphics.UI.WXCore

step1 :: IO ()
step1 =
    do
        -- First, we create a hidden window.  We'll make it visible on the
        -- last step
        win <- frame [text := "wxhNotepad - Step 1", visible := False]

        -- We create the editor
        editor <- textCtrl win [font := fontFixed, -- not really needed, but I like it :)
                                text := "This is our first step in the " ++
                                        "developing of our text editor.\n" ++
                                        "Just a big text area where " ++
                                        "the user can read and write text :)\n" ++
                                        "That's not much, but it's just the " ++
                                        "beginning..."]
        
        -- A simple layout: The whole window filled with the textbox
        --                  with a starting size of 640x480
        set win [layout := fill $ widget editor,
                 clientSize := sz 640 480]
        
        -- Finally we set the focus on the editor
        focusOn editor

        -- HEY, HO... LET'S GO!!
        set win [visible := True]