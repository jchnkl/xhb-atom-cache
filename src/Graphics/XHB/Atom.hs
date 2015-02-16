{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Atom
    ( AtomT
    , MonadAtom(..)
    , runAtomT
    ) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), ask)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, get, gets, modify)
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.HashMap.Lazy (HashMap)
import Data.Typeable (Typeable)
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Data.HashMap.Lazy as M
import qualified Graphics.XHB as X

type AtomStateT m = ReaderT Connection (StateT (HashMap String ATOM) m)

newtype AtomT m a = AtomT { unAtomT :: AtomStateT m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans AtomT where
    lift = AtomT . lift . lift

runAtomT :: Monad m => Connection -> AtomT m a -> m a
runAtomT c = flip evalStateT M.empty . flip runReaderT c . unAtomT

class MonadIO m => MonadAtom m where
    unsafeLookupAtom :: String -> m ATOM

instance MonadIO m => MonadAtom (AtomT m) where
    lookupAtom name = AtomT $ ask >>= \c -> do
        ps <- get
        case M.lookup name ps of
            Just atom -> return (Right atom)
            Nothing -> do
                eatom <- liftIO $ X.internAtom c request >>= X.getReply
                case eatom of
                    Left err   -> return (Left err)
                    Right atom -> do
                        modify $ M.insert name atom
                        return (Right atom)
        where request = MkInternAtom True
                                     (fromIntegral $ length name)
                                     (X.stringToCList name)

    unsafeLookupAtom = AtomT . gets . flip (M.!)

instance (MonadAtom m, MonadTrans t, MonadIO (t m)) => MonadAtom (t m) where
    lookupAtom = lift . lookupAtom
    unsafeLookupAtom = lift . unsafeLookupAtom

instance MonadReader r m => MonadReader r (AtomT m) where
    ask = lift ask
    local f (AtomT m) = AtomT . ReaderT $ local f . runReaderT m

instance MonadState s m => MonadState s (AtomT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AtomT m) where
    tell = lift . tell
    listen = AtomT . listen . unAtomT
    pass = AtomT . pass . unAtomT
