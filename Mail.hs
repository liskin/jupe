module Mail where

import System.Process
import System.IO
import Data.List

sendmail :: [String] -> IO ()
sendmail l = do
    (inp,out,err,pid) <- runInteractiveProcess "/usr/sbin/sendmail"
	["-oem", "-oi", "-t"] Nothing Nothing
    hClose out
    hClose err
    mapM_ (hPutStrLn inp) l
    hClose inp
    waitForProcess pid
    return ()

makemail from rcpt cc bcc subj cont = [
    "From: " ++ from,
    "To: " ++ (concat $ intersperse ", " rcpt),
    "Cc: " ++ (concat $ intersperse ", " cc),
    "Bcc: " ++ (concat $ intersperse ", " bcc),
    "Subject: " ++ subj,
    ""] ++ cont

mail from rcpt cc bcc subj cont =
    sendmail $ makemail from rcpt cc bcc subj cont
