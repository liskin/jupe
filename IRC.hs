module IRC where

import System.IO
import Data.List
import Data.Char

data IRCLine = IRCLine (Maybe String) [String]
    deriving Show

-- | Parse IRC input line.
parseIRCLine l = case l of
    (x:xs) | isSpace x -> error "whitespace at the beginning of input line"
    []                 -> error "empty imput line"
    (':':xs) -> let (src:rest) = ircWords xs in IRCLine (Just src) rest
    xs       -> IRCLine Nothing (ircWords xs)
    where ircWords s = case dropWhile isSpace s of
			    ""       -> []
			    (':':s') -> s' : []
			    s'       -> w : ircWords s''
				        where (w,s'') = break isSpace s'

-- | Make String from an IRC input line.
showIRCLine (IRCLine src vars) =
    concat $ intersperse [' '] (src' ++ init vars ++ [':':last vars])
    where src' = case src of
		    Nothing -> []
		    Just s  -> [ ':':s ]

-- | Strip line ending whitespace.
strip f (x:xs) | f x = case strip f xs of
				[]  -> []
				xs' -> x:xs'
	     | otherwise = x : strip f xs
strip f [] = []

-- | Line ending char?
lineend '\n' = True
lineend '\r' = True
lineend _    = False

-- | End of stats report.
endOfStats dest src st = IRCLine (Just dest)
    ["219", src, st, "End of /STATS report"]

-- | Split by a given delimiter.
splitBy :: Char -> String -> [String]
splitBy c "" = []
splitBy c s  = let (l,s') = break (c==) s
               in l : case s' of []      -> []
                                 (_:s'') -> splitBy c s''
