module Main where

import qualified PodMainGUI
import Paths_Pod(getDataFileName)

main =
    do gladefn <- getDataFileName "data/podresources.glade"
       PodMainGUI.main gladefn
