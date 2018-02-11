-- | ToilT monad transformer. Single-threaded.

module Pos.Txp.Toil.Trans
       (
         -- * Monadic Utxo
         UtxoM
       , utxoGetModified
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo

         -- * Monadic global Toil
       , GlobalToilState (..)
       , GlobalToilM

         -- * Conversions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM

         -- * Old code
       , ToilT
       , runToilTGlobal
       , runToilTLocal
       , execToilTLocal
       , runToilTLocalExtra
       , evalToilTEmpty
       ) where

import           Universum

import           Control.Lens (at, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Data.Default (Default (def))
import qualified Ether
import           Fmt ((+|), (|+))
import           System.Wlog (NamedPureLogger)

import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Class (MonadStakes (..), MonadStakesRead (..))
import           Pos.Txp.Toil.Types (GenericToilModifier (..), MemPool, StakesView, ToilModifier,
                                     UndoMap, UtxoLookup, UtxoModifier, mpLocalTxs, mpSize,
                                     svStakes, svTotal, tmStakes)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGetModified :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGetModified txIn = do
    utxoGet <- ask
    MM.lookup utxoGet txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGetModified id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGetModified id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

type LocalToilM = ReaderT UtxoLookup (State LocalToilState)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: TxId -> LocalToilM Bool
hasTx id = isJust <$> use (ltsMemPool . mpLocalTxs . at id)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: TxId -> TxAux -> TxUndo -> LocalToilM ()
putTxWithUndo id tx undo =
    unlessM (hasTx id) $ do
        ltsMemPool . mpLocalTxs . at id .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at id .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: LocalToilM Int
memPoolSize = use $ ltsMemPool . mpSize

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier   :: !UtxoModifier
    , _gtsStakesModifier :: !StakesView
    }

makeLenses ''GlobalToilState

type GlobalToilM = NamedPureLogger (ReaderT UtxoLookup (State GlobalToilState))





----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: UtxoM a -> LocalToilM a
utxoMToLocalToilM = mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State LocalToilState a
    f = zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: UtxoM a -> GlobalToilM a
utxoMToGlobalToilM = lift . mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State GlobalToilState a
    f = zoom gtsUtxoModifier






----------------------------------------------------------------------------
-- Obsolete
----------------------------------------------------------------------------

-- | Monad transformer which stores ToilModifier and implements
-- writable Toil type classes.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
-- Used for block application now.
type ToilT ext m = Ether.StateT' (GenericToilModifier ext) m

instance MonadStakesRead m => MonadStakesRead (ToilT __ m) where
    getStake id =
        ether $ (<|>) <$> use (tmStakes . svStakes . at id) <*> getStake id
    getTotalStake =
        ether $ maybe getTotalStake pure =<< use (tmStakes . svTotal)

instance MonadStakesRead m => MonadStakes (ToilT __ m) where
    setStake id c = ether $ tmStakes . svStakes . at id .= Just c

    setTotalStake c = ether $ tmStakes . svTotal .= Just c

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

-- | Run ToilT using empty modifier. Should be used for global
-- transaction processing.
runToilTGlobal
    :: (Default ext, Functor m)
    => ToilT ext m a -> m (a, GenericToilModifier ext)
runToilTGlobal txpt = Ether.runStateT' txpt def

-- | Run ToilT using empty stakes modifier. Should be used for local
-- transaction processing.
runToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m (a, ToilModifier)
runToilTLocal um mp undo txpt =
    Ether.runStateT' txpt (def {_tmUtxo = um, _tmMemPool = mp, _tmUndos = undo})

evalToilTEmpty
    :: Monad m
    => ToilT () m a
    -> m a
evalToilTEmpty txpt = Ether.evalStateT txpt def

-- | Execute ToilT using empty stakes modifier. Should be used for
-- local transaction processing.
execToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m ToilModifier
execToilTLocal um mp undo = fmap snd . runToilTLocal um mp undo

-- | Like 'runToilTLocal', but takes extra data as argument.
runToilTLocalExtra
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> extra
    -> ToilT extra m a
    -> m (a, GenericToilModifier extra)
runToilTLocalExtra um mp undo e =
    flip Ether.runStateT' $
        ToilModifier
        { _tmUtxo = um
        , _tmStakes = def
        , _tmMemPool = mp
        , _tmUndos = undo
        , _tmExtra = e
        }
