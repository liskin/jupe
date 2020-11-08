module Main where

import JupeCore
import System.IO
import qualified Network.Simple.TCP as N
import qualified Network.Socket as N
import Control.Exception

import Config (host, port)

import ModBase
import ModJupe
import ModConfCheck

main = bracket connect hClose $ \h -> do
    mods <-
        sequence [
            -- modules go here:
            m newModBase,
            m newModJupe,
            m newModConfCheck
        ]
    JupeConsts { socket = h, modules = mods } `runJupe` do
        allmodsJM mod_init
        forever $ do
            line <- getline
            allmodsJM (mod_input line)

forever x = x >> forever x

connect = do
    (s, _a) <- N.connectSock host (show port)
    N.socketToHandle s ReadWriteMode

m :: (Module a) => IO a -> IO MOD
m = (MOD `fmap`)
