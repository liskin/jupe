import JupeCore
import System.IO
import Network
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
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return h

m :: (Module a) => IO a -> IO MOD
m = (MOD `fmap`)
