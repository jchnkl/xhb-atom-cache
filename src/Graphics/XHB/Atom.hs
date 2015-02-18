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
    ( AtomNameLike(..)
    , AtomT(..)
    , MonadAtom(..)
    , AtomName
    , runAtomT
    , seedAtoms
    , tryLookupAtom
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, get, gets, modify)
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

class (Eq a, Hashable a) => AtomNameLike a where
    toAtomName   :: a -> AtomName
    fromAtomName :: AtomName -> a

instance AtomNameLike String where
    toAtomName   = id
    fromAtomName = id

type AtomState l = (HashMap l ATOM, HashMap ATOM l)

newtype AtomT l m a = AtomT { unAtomT :: StateT (AtomState l) m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans (AtomT l) where
    lift = AtomT . lift

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

runAtomT :: Monad m => AtomT l m a -> m a
runAtomT = flip evalStateT (M.empty, M.empty) . unAtomT

-- | Preseed the atom cache with `ATOM`s
-- Example:
-- @ > let atoms = ["_NET_CLIENT_LIST", "_NET_NUMBER_OF_DESKTOPS"] @
-- @ > fromJust <$> X.connect >>= \c -> runAtomT . seedAtoms c atoms $ mapM_ (\n -> unsafeLookupAtom n >>= liftIO . print) @
seedAtoms :: (AtomNameLike l, Applicative m, MonadIO m)
          => Connection -> [l] -> AtomT l m a -> AtomT l m (Either SomeError a)
seedAtoms _ [] m            = Right <$> m
seedAtoms c ls (AtomT m) = AtomT . runExceptT $ do
    atoms <- mapM eitherToExcept =<< mapM (internAtom c) (map toAtomName ls)
    put (M.fromList $ zip ls atoms, M.fromList $ zip atoms ls)
    lift m

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

-- | Lookup AtomName in cache first, if that fails, try to fetch from the
-- X server and put it into the cache
tryLookupAtom :: (AtomNameLike l, MonadAtom l m, MonadIO m)
              => Connection -> l -> m (Either SomeError ATOM)
tryLookupAtom c l = lookupAtom l >>= \case
    Just a  -> return $ Right a
    Nothing -> runExceptT $ do
        atom <- eitherToExcept =<< internAtom c (toAtomName l)
        insertAtom l atom
        return atom

class (AtomNameLike l, Monad m) => MonadAtom l m where
    insertAtom :: l -> ATOM -> m ()
    lookupAtom :: l -> m (Maybe ATOM)
    lookupName :: ATOM -> m (Maybe l)

instance (AtomNameLike l, Monad m) => MonadAtom l (AtomT l m) where
    insertAtom n a = AtomT . modify $ \(na, an) -> (M.insert n a na, M.insert a n an)
    lookupAtom n = AtomT . gets $ M.lookup n . fst
    lookupName a = AtomT . gets $ M.lookup a . snd

instance MonadError e m => MonadError e (AtomT l m) where
    throwError = lift . throwError
    catchError (AtomT m) f = AtomT $ catchError m (unAtomT . f)

instance (MonadAtom l m, MonadTrans t, Monad (t m)) => MonadAtom l (t m) where
    insertAtom n = lift . insertAtom n
    lookupAtom = lift . lookupAtom
    lookupName = lift . lookupName

instance MonadReader r m => MonadReader r (AtomT l m) where
    ask = lift ask
    local f = AtomT . local f . unAtomT

instance MonadState s m => MonadState s (AtomT l m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AtomT l m) where
    tell = lift . tell
    listen = AtomT . listen . unAtomT
    pass = AtomT . pass . unAtomT
