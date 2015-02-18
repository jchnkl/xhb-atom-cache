{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , tryLookupAtom
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), StateT(..), get, gets, modify)
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word32)
import Data.Hashable (Hashable(..))
import Data.HashMap.Lazy (HashMap)
import Data.Typeable (Typeable)
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Data.HashMap.Lazy as M
import qualified Graphics.XHB as X

instance Hashable ATOM where
    hashWithSalt s a = (s +) . fromIntegral $ (X.fromXid . X.toXid $ a :: Word32)

type AtomName = String

type AtomState = (HashMap AtomName ATOM, HashMap ATOM AtomName)

newtype AtomT m a = AtomT { runAtomT :: StateT AtomState m a }
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
    atoms <- mapM eitherToExcept =<< mapM (internAtom c) names
    put (M.fromList $ zip names atoms, M.fromList $ zip atoms names)
    lift m

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

-- | Lookup AtomName in cache first, if that fails, try to fetch from the
-- X server and put it into the cache
tryLookupAtom :: (MonadAtom m, MonadIO m)
              => Connection -> AtomName -> m (Either SomeError ATOM)
tryLookupAtom c name = lookupAtom name >>= \case
    Just a  -> return $ Right a
    Nothing -> runExceptT $ do
        atom <- eitherToExcept =<< internAtom c name
        insertAtom name atom
        return atom

class Monad m => MonadAtom m where
    insertAtom :: AtomName -> ATOM -> m ()
    lookupAtom :: AtomName -> m (Maybe ATOM)
    lookupName :: ATOM -> m (Maybe AtomName)

instance Monad m => MonadAtom (AtomT m) where
    insertAtom n a = AtomT . modify $ \(na, an) -> (M.insert n a na, M.insert a n an)
    lookupAtom n = AtomT . gets $ M.lookup n . fst
    lookupName a = AtomT . gets $ M.lookup a . snd

instance MonadError e m => MonadError e (AtomT m) where
    throwError = lift . throwError
    catchError (AtomT m) f = AtomT $ catchError m (runAtomT . f)

instance (MonadAtom m, MonadTrans t, Monad (t m)) => MonadAtom (t m) where
    insertAtom n = lift . insertAtom n
    lookupAtom = lift . lookupAtom
    lookupName = lift . lookupName

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
