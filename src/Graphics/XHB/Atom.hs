{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Atom
    ( AtomT(..)
    , MonadAtom(..)
    , runAtomT
    , seedAtoms
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Except
import Control.Monad.Signatures (Catch)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), ask)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, get, gets, modify)
import Control.Monad.Writer (MonadWriter(..))
import Data.HashMap.Lazy (HashMap)
import Data.Typeable (Typeable)
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Data.HashMap.Lazy as M
import qualified Graphics.XHB as X

type AtomName = String

type AtomStateT m = ReaderT Connection (StateT (HashMap AtomName ATOM) m)

newtype AtomT m a = AtomT { unAtomT :: AtomStateT m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans AtomT where
    lift = AtomT . lift . lift

runAtomT :: Monad m => Connection -> AtomT m a -> m a
runAtomT c = flip evalStateT M.empty . flip runReaderT c . unAtomT

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

-- | Preseed the atom cache with `ATOM`s
-- Example:
-- @ > let atoms = ["_NET_CLIENT_LIST", "_NET_NUMBER_OF_DESKTOPS"] @
-- @ > fromJust <$> X.connect >>= \c -> runAtomT c . seedAtoms atoms $ mapM_ (\n -> unsafeLookupAtom n >>= liftIO . print) @
seedAtoms :: (Applicative m, MonadIO m)
          => [AtomName] -> AtomT m a -> AtomT m (Either SomeError a)
seedAtoms []         atom = Right <$> atom
seedAtoms names (AtomT m) = AtomT . runExceptT $ do
    ask >>= forM names . internAtom
        >>= fmap (zip names) . mapM eitherToExcept
        >>= put . M.fromList
    lift m

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

class MonadIO m => MonadAtom m where
    lookupAtom :: AtomName -> m (Either SomeError ATOM)
    unsafeLookupAtom :: AtomName -> m ATOM

instance MonadIO m => MonadAtom (AtomT m) where
    lookupAtom name = AtomT $ ask >>= \c -> do
        ps <- get
        case M.lookup name ps of
            Just atom -> return (Right atom)
            Nothing -> internAtom c name >>= \case
                Left err   -> return (Left err)
                Right atom -> do
                    modify $ M.insert name atom
                    return (Right atom)

    unsafeLookupAtom = AtomT . gets . flip (M.!)

liftCatch :: Monad m => Catch e m a -> Catch e (AtomT m) a
liftCatch f (AtomT m) h = undefined
    AtomT . ReaderT $ \r -> StateT $ \s -> do
        a <- f (evalStateT (runReaderT m r) s) (runAtomT r . h)
        return (a, s)

instance MonadError e m => MonadError e (AtomT m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

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
