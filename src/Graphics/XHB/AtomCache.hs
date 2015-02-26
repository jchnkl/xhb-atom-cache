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

module Graphics.XHB.AtomCache
    ( AtomId(..)
    , AtomLike(..)
    , AtomCacheT(..)
    , AtomCacheCtx(..)
    , AtomName
    , atomName
    , runAtomCacheT
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
    fromAtom (AtomId a) = cast a

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

type AtomCache = (HashMap AtomId ATOM, HashMap ATOM AtomId)

newtype AtomCacheT m a = AtomCacheT { unAtomT :: StateT AtomCache m a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans AtomCacheT where
    lift = AtomCacheT . lift

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

runAtomCacheT :: Monad m => AtomCacheT m a -> m a
runAtomCacheT = flip evalStateT (M.empty, M.empty) . unAtomT

-- | Preseed the atom cache with `AtomLike`s. Example:
--
-- @
-- {-\# LANGUAGE DeriveDataTypeable \#-}
--
-- import Data.Typeable (Typeable)
-- import Data.Hashable (Hashable(..))
-- import Control.Monad (forM_, void)
-- import Control.Monad.IO.Class (liftIO)
-- import Graphics.XHB (connect)
-- import Graphics.XHB.AtomCache
--
-- data ATOMS = NET_CLIENT_LIST | NET_NUMBER_OF_DESKTOPS
--     deriving (Eq, Show, Typeable)
--
-- instance Hashable ATOMS where
--     hashWithSalt s = hashWithSalt s . show
--
-- instance AtomLike ATOMS where
--     toAtomName a = '_' : show a
--
-- atoms :: [ATOMS]
-- atoms = [NET_CLIENT_LIST, NET_NUMBER_OF_DESKTOPS]
--
-- main :: IO ()
-- main = do
--     Just c <- connect
--     void $ runAtomCacheT . seedAtoms c atoms $ do
--         forM_ atoms $ \\a -> unsafeLookupATOM a >>= liftIO . print
-- @
seedAtoms :: (AtomLike l, Applicative m, MonadIO m)
          => Connection -> [l] -> AtomCacheT m a -> AtomCacheT m (Either SomeError a)
seedAtoms _ [] m         = Right <$> m
seedAtoms c as (AtomCacheT m) = AtomCacheT . runExceptT $ do
    atoms <- mapM eitherToExcept =<< mapM (internAtom c) (map toAtomName as)
    modify $ \(f, s) -> (f `M.union` fs atoms, s `M.union` ss atoms)
    lift m
    where
    atomids = map toAtom as
    fs = M.fromList . zip atomids
    ss = M.fromList . flip zip atomids

internAtom :: MonadIO m => Connection -> AtomName -> m (Either SomeError ATOM)
internAtom c name = liftIO $ X.internAtom c request >>= X.getReply
    where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

-- | Lookup AtomName in cache first, if that fails, try to fetch from the
-- X server and put it into the cache
tryLookupAtom :: (AtomLike l, AtomCacheCtx m, MonadIO m)
              => Connection -> l -> m (Either SomeError ATOM)
tryLookupAtom c a = lookupATOM a >>= \case
    Just atom  -> return $ Right atom
    Nothing    -> runExceptT $ do
        atom <- eitherToExcept =<< internAtom c (toAtomName a)
        insertATOM a atom
        return atom

class Monad m => AtomCacheCtx m where
    insertATOM :: AtomLike l => l -> ATOM -> m ()
    lookupATOM :: AtomLike l => l -> m (Maybe ATOM)
    unsafeLookupATOM :: AtomLike l => l -> m ATOM
    lookupAtomId :: ATOM -> m (Maybe AtomId)
    unsafeLookupAtomId :: ATOM -> m AtomId

instance Monad m => AtomCacheCtx (AtomCacheT m) where
    insertATOM n a = AtomCacheT . modify $ \(na, an) -> (M.insert (toAtom n) a na, M.insert a (toAtom n) an)
    lookupATOM n = AtomCacheT . gets $ M.lookup (toAtom n) . fst
    unsafeLookupATOM n = AtomCacheT . gets $ (M.! (toAtom n)) . fst
    lookupAtomId a = AtomCacheT . gets $ M.lookup a . snd
    unsafeLookupAtomId a = AtomCacheT . gets $ (M.! a) . snd

instance MonadError e m => MonadError e (AtomCacheT m) where
    throwError = lift . throwError
    catchError (AtomCacheT m) f = AtomCacheT $ catchError m (unAtomT . f)

instance (AtomCacheCtx m, MonadTrans t, Monad (t m)) => AtomCacheCtx (t m) where
    insertATOM n = lift . insertATOM n
    lookupATOM = lift . lookupATOM
    unsafeLookupATOM = lift . unsafeLookupATOM
    lookupAtomId = lift . lookupAtomId
    unsafeLookupAtomId = lift . unsafeLookupAtomId

instance MonadReader r m => MonadReader r (AtomCacheT m) where
    ask = lift ask
    local f = AtomCacheT . local f . unAtomT

instance MonadState s m => MonadState s (AtomCacheT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AtomCacheT m) where
    tell = lift . tell
    listen = AtomCacheT . listen . unAtomT
    pass = AtomCacheT . pass . unAtomT
