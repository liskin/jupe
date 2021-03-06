module ModBase (newModBase) where

import JupeCore
import Data.Maybe
import IRC (endOfStats)
import Config (server, serverpass, serversid, remote)

newModBase :: IO ModBase
newModBase = return ModBase

data ModBase = ModBase
instance Module ModBase where
    mod_init _ = do
        putline $ IRCLine Nothing ["PASS", serverpass, "TS", "6", serversid]
        putline $ IRCLine Nothing ["SERVER", server, "1", "Jupe server"]

    mod_input i@(IRCLine _ (cmd:_)) _ =
        case cmd of
             "PING" -> ping i
             "STATS" -> stats i
             _ -> return ()

-- | Ping.
ping (IRCLine src (_:argv)) = do
    let dest = case argv of
                    (_:d:_) -> d
                    _       -> server
        destp = if dest == serversid then server else dest
    putline $ IRCLine (Just dest) (["PONG", destp] ++ maybeToList src)
ping _ = return ()

-- | Stats.
stats (IRCLine (Just src) (_:st:dest:[])) = do
    case st of
         "c" -> do putline $ IRCLine (Just dest) ["213", src, "C",
                       "*@127.0.0.1", "*",
                       if dest == server then remote else server,
                       "65536", "server"]
                   putline $ endOfStats dest src st
         _ -> return ()
stats _ = return ()
