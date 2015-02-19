{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Atom
    ( AtomId(..)
    , AtomLike(..)
    , AtomT(..)
    , MonadAtom(..)
    , AtomName
    , atomName
    , runAtomT
    , seedAtoms
    , tryLookupAtom
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (join)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT, get, gets, modify)
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word32)
import Data.Hashable (Hashable(..))
import Data.HashMap.Lazy (HashMap)
import Data.Typeable (Typeable, cast)
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Data.HashMap.Lazy as M
import qualified Graphics.XHB as X

-- TODO: pull in to Graphics.XHB repo
instance Hashable ATOM where
    hashWithSalt s a = (s +) . fromIntegral $ (X.fromXid . X.toXid $ a :: Word32)

class (Eq l, Hashable l, Typeable l) => AtomLike l where
    toAtom :: l -> AtomId
    toAtom = AtomId

    fromAtom :: AtomId -> Maybe l
    fromAtom = cast

    toAtomName :: l -> AtomName

atomName :: AtomId -> AtomName
atomName (AtomId a) = toAtomName a

data AtomId = forall l. AtomLike l => AtomId l
    deriving Typeable

instance Eq AtomId where
    AtomId a == AtomId b = maybe False (b ==) (cast a)

instance Hashable AtomId where
    hashWithSalt s (AtomId a) = hashWithSalt s a

type AtomName = String

type AtomState = (HashMap AtomId ATOM, HashMap ATOM AtomId)

newtype AtomT m a = AtomT { unAtomT :: StateT AtomState m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans AtomT where
    lift = AtomT . lift

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

runAtomT :: Monad m => AtomT m a -> m a
runAtomT = flip evalStateT (M.empty, M.empty) . unAtomT

-- | Preseed the atom cache with `ATOM`s
-- Example:
-- @ > let atoms = ["_NET_CLIENT_LIST", "_NET_NUMBER_OF_DESKTOPS"] @
-- @ > fromJust <$> X.connect >>= \c -> runAtomT . seedAtoms c atoms $ mapM_ (\n -> unsafeLookupAtom n >>= liftIO . print) @
seedAtoms :: (AtomLike l, Applicative m, MonadIO m)
          => Connection -> [l] -> AtomT m a -> AtomT m (Either SomeError a)
seedAtoms _ [] m         = Right <$> m
seedAtoms c as (AtomT m) = AtomT . runExceptT $ do
    atoms <- mapM eitherToExcept =<< mapM (internAtom c) (map toAtomName as)
    put (M.fromList $ zip atomids atoms, M.fromList $ zip atoms atomids)
    lift m
    where atomids = map toAtom as

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

-- | Lookup AtomName in cache first, if that fails, try to fetch from the
-- X server and put it into the cache
tryLookupAtom :: (AtomLike l, MonadAtom m, MonadIO m)
              => Connection -> l -> m (Either SomeError ATOM)
tryLookupAtom c a = lookupATOM a >>= \case
    Just atom  -> return $ Right atom
    Nothing    -> runExceptT $ do
        atom <- eitherToExcept =<< internAtom c (toAtomName a)
        insertATOM a atom
        return atom

class Monad m => MonadAtom m where
    insertATOM :: AtomLike l => l -> ATOM -> m ()
    lookupATOM :: AtomLike l => l -> m (Maybe ATOM)
    lookupAtomId :: AtomLike l => ATOM -> m (Maybe l)

instance Monad m => MonadAtom (AtomT m) where
    insertATOM n a = AtomT . modify $ \(na, an) -> (M.insert (toAtom n) a na, M.insert a (toAtom n) an)
    lookupATOM n = AtomT . gets $ M.lookup (toAtom n) . fst
    lookupAtomId a = AtomT . gets $ join . fmap fromAtom . M.lookup a . snd

instance MonadError e m => MonadError e (AtomT m) where
    throwError = lift . throwError
    catchError (AtomT m) f = AtomT $ catchError m (unAtomT . f)

instance (MonadAtom m, MonadTrans t, Monad (t m)) => MonadAtom (t m) where
    insertATOM n = lift . insertATOM n
    lookupATOM = lift . lookupATOM
    lookupAtomId = lift . lookupAtomId

instance MonadReader r m => MonadReader r (AtomT m) where
    ask = lift ask
    local f = AtomT . local f . unAtomT

instance MonadState s m => MonadState s (AtomT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AtomT m) where
    tell = lift . tell
    listen = AtomT . listen . unAtomT
    pass = AtomT . pass . unAtomT
