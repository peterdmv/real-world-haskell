module PodMainGUI where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket (withSocketsDo)

-- GUI libraries
import Graphics.UI.Gtk hiding (disconnect, add)
import Graphics.UI.Gtk.Builder

-- Threading
import Control.Concurrent

-- | Our main GUI type
data GUI = GUI {
    mainWin :: Window
  , mwAddBt :: Button
  , mwUpdateBt :: Button
  , mwDownloadBt :: Button
  , mwFetchBt :: Button
  , mwExitBt :: Button
  , statusWin :: Dialog
  , swOKBt :: Button
  , swCancelBt :: Button
  , swLabel :: Label
  , addWin :: Dialog
  , awOKBt :: Button
  , awCancelBt :: Button
  , awEntry :: Entry}

{-
- 'main' takes a 'FilePath' as argument due to how its called in
- PodLocalMain with the argument "podresources.glade"
-}
main :: FilePath -> IO ()
main gladepath = withSocketsDo $ do
    -- Initialize GTK+ engine
    initGUI

    -- Every so often, we try to run other threads
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    -- Load the GUI from the Glade file
    gui <- loadGlade gladepath

    -- Connect to the database
    dbh <- connect "poddbtest"

    -- Set up our events
    connectGui gui dbh

    -- Run the GTK+ main loop, exits after GUI is done
    mainGUI

    -- Disconnect from database at the end
    disconnect dbh

loadGlade :: FilePath -> IO GUI
loadGlade gladepath = do

    -- Load XML from glad path.
    -- Note: crashes with a runtime error on console if fails!
    --Just xml <- xmlNew gladepath
    builder <- builderNew
    builderAddFromFile builder gladepath

    -- Load main window
    --mw <- xmlGetWidget xml castToWindow "mainWindow"
    mw <- builderGetObject builder castToWindow "mainWindow"

    -- Load all buttons
    -- [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
    -- auOK, auCancel] <- mapM (xmlGetWidget xml castToButton)
    -- ["addButton", "updateButton", "downloadButton",
    -- "fetchButton", "exitButton", "okButton",
    -- "cancelButton", "auOK", "auCancel"]
    [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
     auOK, auCancel] <- mapM (builderGetObject builder castToButton)
                        ["addButton", "updateButton", "downloadButton",
                         "fetchButton", "exitButton", "okButton",
                         "cancelButton", "auOK", "auCancel"]

    -- sw <- xmlGetWidget xml castToDialog "statusDialog"
    -- swl <- xmlGetWidget xml castToLabel "statusLabel"

    -- au <- xmlGetWidget xml castToDialog "addDialog"
    -- aue <- xmlGetWidget xml castToEntry "auEntry"

    sw <- builderGetObject builder castToDialog "statusDialog"
    swl <- builderGetObject builder castToLabel "statusLabel"

    au <- builderGetObject builder castToDialog "addDialog"
    aue <- builderGetObject builder castToEntry "auEntry"

    return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit
           sw swOK swCancel swl au auOK auCancel aue

connectGui :: IConnection conn => GUI -> conn -> IO ()
connectGui gui dbh = do
    -- When the close button is clicked, termainate GUI loop by calling GTK
    -- mainQuit function
    onDestroy (mainWin gui) mainQuit

    -- Main window buttons
    onClicked (mwAddBt gui) (guiAdd gui dbh)
    onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
    onClicked (mwDownloadBt gui) (guiDownload gui dbh)
    onClicked (mwFetchBt gui) (guiFetch gui dbh)
    onClicked (mwExitBt gui) mainQuit
    return ()

guiAdd :: IConnection conn => GUI -> conn -> IO ()
guiAdd gui dbh = do
    -- Initialize the add URL window
    entrySetText (awEntry gui) ""
    onClicked (awCancelBt gui) (widgetHide $ addWin gui)
    onClicked (awOKBt gui) procOK

    -- Show the add URL window
    windowPresent $ addWin gui

  where procOK = do
          url <- entryGetText $ awEntry gui
          widgetHide $ addWin gui -- Remove the dialog
          add dbh url -- Add to the DB

statusWindow :: IConnection conn =>
                GUI
             -> conn
             -> String
             -> ((String -> IO ()) -> IO ())
             -> IO ()
statusWindow gui dbh title func = do
    -- Clear the status text
    labelSetText (swLabel gui) ""

    -- Disable the OK button, enable Cancel button
    widgetSetSensitivity (swOKBt gui) False
    widgetSetSensitivity (swCancelBt gui) True

    -- Set the title
    windowSetTitle (statusWin gui) title

    -- Start the operation
    childThread <- forkIO childTasks

    -- Define what happens when clicking on Cancel
    onClicked (swCancelBt gui) (cancelChild childThread)

    -- Show the window
    windowPresent $ statusWin gui

  where childTasks = do
          updateLabel "Starting thread..."
          func updateLabel

          -- After the child task finishes, enable OK and disable Cancel
          enableOK

        enableOK = do
          widgetSetSensitivity (swCancelBt gui) False
          widgetSetSensitivity (swOKBt gui) True
          onClicked (swOKBt gui) (widgetHide $ statusWin gui)
          return ()

        updateLabel text = labelSetText (swLabel gui) text

        cancelChild childThread = do
          killThread childThread
          yield
          updateLabel "Action has been cancelled."
          enableOK

guiUpdate, guiDownload, guiFetch :: IConnection conn => GUI -> conn -> IO ()
guiUpdate gui dbh = statusWindow gui dbh "Pod: Update" (update dbh)

guiDownload gui dbh = statusWindow gui dbh "Pod: Download" (download dbh)

guiFetch gui dbh = statusWindow gui dbh "Pod: Fetch"
    (\logf -> update dbh logf >> download dbh logf)

add :: IConnection conn => conn -> String -> IO ()
add dbh url = do
    addPodcast dbh pc
    commit dbh
  where pc = Podcast { castId = 0
                     , castURL = url
                     }

update :: IConnection conn => conn -> (String -> IO ()) -> IO ()
update dbh logf = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    logf "Update complete."
  where procPodcast pc = do
          logf $ "Updating from " ++ (castURL pc)
          updatePodcastFromFeed dbh pc

download :: IConnection conn => conn -> (String -> IO ()) -> IO ()
download dbh logf = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    logf "Download complete."
  where procPodcast pc = do
          logf $ "Considering " ++ (castURL pc)
          episodelist <- getPodcastEpisodes dbh pc
          let dleps = filter (\ep -> epDone ep == False) episodelist
          mapM_ procEpisode dleps

        procEpisode ep = do
          logf $ "Downloading " ++ (epURL ep)
          getEpisode dbh ep
