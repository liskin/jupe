{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module JupeCore (
    module JupeCore,
    IRCLine(..),
    ) where

import Control.Monad.Reader
import System.IO
import IRC

-- the module system:

data MOD = forall a. (Module a) => MOD a

-- | Apply a given function to a module in the existential type.
modap           :: (forall a. (Module a) => a -> b) -> MOD -> b
modap f (MOD x) = f x


-- | The module interface.
class Module a where
    mod_init   :: a -> JupeM ()
    mod_init _ = return ()

    mod_input     :: IRCLine -> a -> JupeM ()
    mod_input _ _ = return ()



-- the JupeM monad:

type JupeM = ReaderT JupeConsts IO

data JupeConsts = JupeConsts {
    socket :: Handle,
    modules :: [MOD]
}



-- helper functions:

io :: (MonadIO m) => IO a -> m a
io = liftIO

runJupe = flip runReaderT

-- Run a given action for all modules.
allmodsJM :: (forall a. (Module a) => a -> JupeM b) -> JupeM ()
allmodsJM f = asks modules >>= mapM_ (modap f)

-- Get one line.
getline :: JupeM IRCLine
getline = do
    l <- asks socket >>= io . (strip lineend `fmap`) . hGetLine
    (io $ putStr "<--- ") >> (io $ print l)
    return $ parseIRCLine l

-- Put one line.
putline :: IRCLine -> JupeM ()
putline l = do
    let l' = showIRCLine l
    asks socket >>= io . (`hPutStrLn` l')
    (io $ putStr "---> ") >> (io $ print l')
