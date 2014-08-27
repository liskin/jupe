module ModJupe (newModJupe, jupe, squit) where

import JupeCore
import Config (server)

newModJupe :: IO ModJupe
newModJupe = return ModJupe

data ModJupe = ModJupe
instance Module ModJupe where
    mod_input i@(IRCLine _ (cmd:_)) _ =
        case cmd of
             "CONNECT" -> connect i
             _ -> return ()

-- | Connect.
connect (IRCLine (Just src) (_:target:_:srv:[])) = do
    check1 $ check2 $ ok
    where check1 =
              if ('*' `elem` target) || ('?' `elem` target)
                 then const $ putline $ IRCLine (Just server)
                         ["NOTICE", src, "Jupe cannot contain wildcards"]
                 else id
          check2 =
              if srv /= server
                 then const $ putline $ IRCLine (Just server)
                         ["NOTICE", src, "Only " ++ server ++ " can jupe"]
                 else id
          ok = jupe target src ""
connect _ = return ()

jupe srv src reason = do
    putline $ IRCLine (Just server)
        ["SERVER", srv, "2", "Juped by " ++ src ++ "; " ++ reason]
    putline $ IRCLine (Just server)
        ["WALLOPS", srv ++ " juped by " ++ src ++ "; " ++ reason]

squit srv src reason = do
    putline $ IRCLine (Just server)
        ["SQUIT", srv, "Juped by " ++ src ++ "; " ++ reason]
