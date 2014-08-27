{-# LANGUAGE ScopedTypeVariables #-}

module ModConfCheck (newModConfCheck) where

import JupeCore
import Config (server, remote, jupenick)
import IRC (strip, splitBy)
import Data.Char
import Text.Regex.Posix
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Mail
import ModJupe (jupe, squit)
import Data.Time.Clock.POSIX
import System.IO.Unsafe

newModConfCheck :: IO ModConfCheck
newModConfCheck = do
    conf <- newIORef Map.empty
    return ModConfCheck {
        conf = conf
    }

data ModConfCheck = ModConfCheck {
    conf :: IORef (Map String Server)
}
instance Module ModConfCheck where
    mod_init _ = do
        putline $ IRCLine (Just server) ["NICK", jupenick, "1", "1",
            "+aiow", jupenick, "127.0.0.1", server, "i'm shadow"]
    mod_input i@(IRCLine _ (cmd:_)) m =
        case cmd of
             "SERVER" -> server_ i m
             "SID" -> server_ i m
             "371" -> infoLine i m
             "105" -> iSupport i m
             "374" -> endOfInfo i m
             "257" -> adminLine i m
             "258" -> adminLine i m
             "259" -> adminLine i m
             "248" -> sharedStats i m
             "215" -> authLine i m
             _ -> return ()

data Server = Server {
    srv_config :: [(String, String)],
    srv_admins :: [String],
    srv_ilines :: [[String]],
    srv_shared_ok :: Bool
} deriving Show

newServer = Server [] [] [] False
srv_config_add y@(Server { srv_config = x }) z = y { srv_config = z : x }
srv_admins_add y@(Server { srv_admins = x }) z = y { srv_admins = z : x }
srv_ilines_add y@(Server { srv_ilines = x }) z = y { srv_ilines = z : x }
srv_shared_ok_set y _ = y { srv_shared_ok = True }

-- | Server.
server_ (IRCLine _ (_:srv:_)) m = startCheck srv m
server_ _ _ = return ()

-- | Initiate check.
startCheck srv m = do
    putline $ IRCLine (Just jupenick) ["ADMIN", srv]
    putline $ IRCLine (Just jupenick) ["VERSION", srv]
    putline $ IRCLine (Just jupenick) ["STATS", "U", srv]
    putline $ IRCLine (Just jupenick) ["STATS", "i", srv]
    putline $ IRCLine (Just jupenick) ["INFO", srv]
    io $ conf m `modifyIORef` Map.alter (const $ Just newServer) srv

-- | Parse info line.
infoLine (IRCLine (Just srv) (_:_:l:[])) m = do
    case l =~~ "^(\\w+) +(.+) \\[.*\\]" of
         Just (_::String,_::String,_::String,[var::String,val]) ->
             save m srv srv_config_add (var, (strip isSpace val))
         _ -> return ()
infoLine _ _ = return ()

-- | iSupport.
iSupport (IRCLine (Just srv) (_:_:l)) m =
    mapM_ one l
    where one s = case splitBy '=' s of
                    [var, val] -> save m srv srv_config_add (var, val)
                    _ -> return ()
iSupport _ _ = return ()

-- | Admin.
adminLine (IRCLine (Just srv) (_:_:l:[])) m = do
    let admins = l =~ "<[[:alnum:].]+@[[:alnum:].]+>|^[[:alnum:].]+@[[:alnum:].]+$"
    mapM_ (save m srv srv_admins_add) (map head admins)
adminLine _ _ = return ()

-- | shared.
sharedStats (IRCLine (Just srv) (_:_:"U":"*":mask:_)) m =
    if mask `elem` ["*@*", "<NULL>@<NULL>"]
       then save m srv srv_shared_ok_set ()
       else return ()
sharedStats _ _ = return ()

-- | ilines.
authLine (IRCLine (Just srv) (_:_:"I":l)) m =
    save m srv srv_ilines_add l
authLine _ _ = return ()

-- | End of info.
endOfInfo (IRCLine (Just srv) _) m = do
    conf' <- io $ readIORef $ conf m
    let (info :: Maybe Server) = srv `Map.lookup` conf'
    case info of
         Nothing -> return ()
         Just xx -> runCheck srv xx
    io $ conf m `modifyIORef` Map.delete srv
endOfInfo _ _ = return ()

-- | Save a piece of information using a given modifier.
save m srv f v =
    io $ conf m `modifyIORef` Map.alter (\x ->
        case x of
             Nothing -> Nothing
             Just sv -> Just $ f sv v
        ) srv

-- | Run all the checks.
check srv =
    cShared ++ concatMap cAuth (srv_ilines srv) ++ concatMap cSetting (srv_config srv)
    where cShared = if not (srv_shared_ok srv)
                       then [ ("Spatne nastaveny blok shared {}", sharedcrit) ]
                       else []
          cSetting (var, val) =
              case lookup var config_settings of
                   Just (val', crit) -> if val /= val'
                       then [ ("Konf. volba " ++ var ++ " je spatne; ma byt: "
                              ++ val'', crit) ]
                       else []
                       where val'' = if val' == "NONE" then "<prazdne>" else val'
                   Nothing -> []
          cAuth l =
              case lookup l banned_ilines of
                   Just crit -> [ ("ILine \"" ++ unwords l ++ "\" je zakazana", crit) ]
                   Nothing -> []

-- | Run the check and send email.
runCheck name srv = case check srv of
    [] -> return ()
    xs -> do
        critical <- or `fmap` mapM (io . crit . snd) xs
        io $ mail "irc-jupe@tomi.nomi.cz" (srv_admins srv) ["irc@tomi.nomi.cz"] []
            ("Chyba v nastaveni IRC serveru " ++ name)
            ([
                "Ahoj,",
                "",
                "bylo zjisteno, ze nastaveni serveru " ++ name ++
                    " ma tyto nedostatky:", ""
             ] ++
             map (("    "++) . fst) xs ++
             [""] ++
             (if not critical then [] else ["Nektere z nich jsou zavazne, a proto byl server odpojen.",""]) ++
             ["Informace o spravnem nastaveni jsou zde: http://irc.nomi.cz/ratboxsetup.html"
             ,"Neridte se prosim jen temito pokyny, u nastaveni serveru premyslejte."
             ,"Nedari-li se vam to, najdete si za sebe nahradu."
             ,""
             ,"Dekujeme, sprava IRC site."]
            )
        if critical && name /= remote
           then do
               squit name jupenick "critical configuration problem"
               jupe name jupenick "critical configuration problem"
           else return ()

type Crit = Maybe POSIXTime

crit :: Crit -> IO Bool
crit (Just t) = do
    now <- getPOSIXTime
    return $ now > t
crit Nothing = return False

sharedcrit :: Crit
sharedcrit = Just 1191147064

config_settings :: [(String, (String, Crit))]
config_settings = [
        ("NICKLEN",                     ("20",          (Just 1191147064))),
        ("CHANNELLEN",                  ("50",          (Just 1191147064))),
        ("TOPICLEN",                    ("390",         Nothing)),
        ("ts_max_delta",                ("300",         (Just 1191147064))),
        ("network_name",                ("CZFree",      Nothing)),
        ("max_chans_per_user",          ("50",          (Just 1191147064))),
        ("max_bans",                    ("42",          (Just 1191147064))),
        ("kline_reason",                ("NONE",        Nothing)),
        ("min_nonwildcard",             ("2",           Nothing)),
        ("min_nonwildcard_simple",      ("2",           Nothing))
    ]

banned_ilines :: [([String], Crit)]
banned_ilines =
    [ (["irc.fi", "*", "*@*.fi", "6667", "users"],      (Just 1218633298))
    , (["NOMATCH", "*", "*@0.0.0.0/0", "0", "opers"],   (Just 1218633298))
    , (["NOMATCH", "*", "*@*", "0", "opers"],           (Just 1218633298))
    ]
