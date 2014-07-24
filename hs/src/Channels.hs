{-# OPTIONS_GHC -Wall #-}

module Channels ( chanRc
                , chanSimTelem
                , chanSensors
                , chanDynoPlot
                , chanOptTelem
                ) where

chanRc :: String
chanRc = "ipc:///tmp/rc"

chanSimTelem :: String
chanSimTelem = "ipc:///tmp/simtelem"

chanSensors :: String
chanSensors = "ipc:///tmp/sensors"

chanDynoPlot :: String
chanDynoPlot = "ipc:///tmp/dynoplot"

chanOptTelem :: String
chanOptTelem = "ipc:///tmp/opttelem"
