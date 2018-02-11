{-# LANGUAGE TypeFamilies #-}

-- | Instance of 'MonadStakesRead' which uses DB.

module Pos.Txp.Toil.DBToil
       ( DBToil
       , runDBToil
       ) where

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Ether

import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Stakes (getRealStake, getRealTotalStake)
import           Pos.Txp.Toil.Class (MonadStakesRead (..))

data DBToilTag

type DBToil = Ether.TaggedTrans DBToilTag IdentityT

runDBToil :: DBToil m a -> m a
runDBToil = coerce

instance (MonadDBRead m) => MonadStakesRead (DBToil m) where
    getTotalStake = getRealTotalStake
    getStake = getRealStake
