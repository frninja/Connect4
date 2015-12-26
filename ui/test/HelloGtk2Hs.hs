module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

 main = do
	initGUI
    Just xml    <- xmlNew "hellogtk2hs.glade"
    window      <- xmlGetWidget xml castToWindow "window1"
    onDestroy window mainQuit
    closeButton <- xmlGetWidget xml castToButton "button2"
    onClicked closeButton $ do
        widgetDestroy window
    label       <- xmlGetWidget xml castToLabel "label1"
    entry       <- xmlGetWidget xml castToEntry "entry1"
    applyButton <- xmlGetWidget xml castToButton "button1"
    onClicked applyButton $ do
        name <- get entry entryText
        set label [ labelText := "Hello " ++ name ]
    widgetShowAll window
    mainGUI