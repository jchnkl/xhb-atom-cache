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
    , AtomName
    , seedAtoms
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Except
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), StateT(..), get, gets, modify)
import Control.Monad.Writer (MonadWriter(..))
import Data.HashMap.Lazy (HashMap)
import Data.Typeable (Typeable)
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Data.HashMap.Lazy as M
import qualified Graphics.XHB as X

type AtomName = String

newtype AtomT m a = AtomT { runAtomT :: StateT (HashMap AtomName ATOM) m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans AtomT where
    lift = AtomT . lift

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

-- | Preseed the atom cache with `ATOM`s
-- Example:
-- @ > let atoms = ["_NET_CLIENT_LIST", "_NET_NUMBER_OF_DESKTOPS"] @
-- @ > fromJust <$> X.connect >>= \c -> runAtomT . seedAtoms c atoms $ mapM_ (\n -> unsafeLookupAtom n >>= liftIO . print) @
seedAtoms :: (Applicative m, MonadIO m)
          => Connection -> [AtomName] -> AtomT m a -> AtomT m (Either SomeError a)
seedAtoms _ [] m            = Right <$> m
seedAtoms c names (AtomT m) = AtomT . runExceptT $ do
    forM names (internAtom c)
        >>= fmap (zip names) . mapM eitherToExcept
        >>= put . M.fromList
    lift m

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

class MonadIO m => MonadAtom m where
    lookupAtom :: Connection -> AtomName -> m (Either SomeError ATOM)
    unsafeLookupAtom :: AtomName -> m ATOM

instance MonadIO m => MonadAtom (AtomT m) where
    lookupAtom c name = AtomT $ do
        ps <- get
        case M.lookup name ps of
            Just atom -> return (Right atom)
            Nothing -> internAtom c name >>= \case
                Left err   -> return (Left err)
                Right atom -> do
                    modify $ M.insert name atom
                    return (Right atom)

    unsafeLookupAtom = AtomT . gets . flip (M.!)

instance MonadError e m => MonadError e (AtomT m) where
    throwError = lift . throwError
    catchError (AtomT m) f = AtomT $ catchError m (runAtomT . f)

instance (MonadAtom m, MonadTrans t, MonadIO (t m)) => MonadAtom (t m) where
    lookupAtom c = lift . lookupAtom c
    unsafeLookupAtom = lift . unsafeLookupAtom

instance MonadReader r m => MonadReader r (AtomT m) where
    ask = lift ask
    local f = AtomT . local f . runAtomT

instance MonadState s m => MonadState s (AtomT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AtomT m) where
    tell = lift . tell
    listen = AtomT . listen . runAtomT
    pass = AtomT . pass . runAtomT
