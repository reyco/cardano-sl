{-# LANGUAGE TypeFamilies #-}

-- | Type classes for Toil abstraction.
-- * MonadUtxoRead and MonadUtxo for encapsulation of Utxo storage.
-- * MonadStakesRead and MonadStakes for encapsulation of Stakes storage.
-- * MonadTxPoll for encapsulation of mem pool of local transactions.

module Pos.Txp.Toil.Class
       ( MonadUtxoRead (..)
       , utxoGetReader
       , MonadStakesRead (..)
       , MonadStakes (..)
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Trans.Class (MonadTrans)

import           Pos.Core (Coin, HasConfiguration, StakeholderId)
import           Pos.Core.Txp (TxIn, TxOutAux)
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Util.Util (HasLens', ether, lensOf)

----------------------------------------------------------------------------
-- MonadUtxo
----------------------------------------------------------------------------

class (HasConfiguration, Monad m) => MonadUtxoRead m where
    utxoGetObsolete :: TxIn -> m (Maybe TxOutAux)

instance {-# OVERLAPPABLE #-}
    (MonadUtxoRead m, MonadTrans t, Monad (t m)) =>
        MonadUtxoRead (t m) where
  utxoGetObsolete = lift . utxoGetObsolete

-- | Implementation of 'utxoGet' which takes data from 'MonadReader' context.
utxoGetReader :: (HasLens' ctx Utxo, MonadReader ctx m) => TxIn -> m (Maybe TxOutAux)
utxoGetReader txIn = view $ lensOf @Utxo . at txIn

instance HasConfiguration => MonadUtxoRead ((->) Utxo) where
    utxoGetObsolete = utxoGetReader

----------------------------------------------------------------------------
-- MonadStakes
----------------------------------------------------------------------------

class Monad m => MonadStakesRead m where
    getStake :: StakeholderId -> m (Maybe Coin)
    getTotalStake :: m Coin

instance {-# OVERLAPPABLE #-}
    (MonadStakesRead m, MonadTrans t, Monad (t m)) =>
        MonadStakesRead (t m)
  where
    getStake = lift . getStake
    getTotalStake = lift getTotalStake

class MonadStakesRead m => MonadStakes m where
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadStakes m, MonadTrans t, Monad (t m)) =>
        MonadStakes (t m)
  where
    setStake id = lift . setStake id
    setTotalStake = lift . setTotalStake
