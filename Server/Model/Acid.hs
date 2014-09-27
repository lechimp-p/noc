module ACID 
    ( runQuery
    , runQueryAndUpdate
    , runSimple
    )
where

import Model.BaseTypes
import Model.Errors
import Model.Query
import Model.Update
import Model.Exec

import Model.Simple.NoC (NoC)
import qualified Model.Acid.Query as AQ

{--import qualified Model.Simple.NoC as N
import qualified Model.Simple.User as U
import Model.Simple.User
import qualified Model.Simple.Channel as C
import Model.Simple.Channel
import qualified Model.Simple.Message as M
import Model.Simple.Message
--}
import Control.Lens 
import Control.Eff
import Control.Eff.Lift
import qualified Data.Acid as A
import qualified Data.Acid.Advanced as AA
import qualified Data.Set as S 
import qualified Data.IxSet as IX
import Data.Time.Clock (UTCTime)

data Effect
    = Go (VE r w)
    | Lift (m (Either Error a)) (a -> Either Error (VE r w))
    | Throw Error

runQuery :: (MonadIO m, SetMember Lift (Lift m) r)
         => A.AcidState NoC -> Eff (Query :> Exec :> r) -> Eff (Exec :> r) a
runQuery state action = go state (admin action)
    where
    go _ (Val c) = return v
    go noc (E request) = handleRelay request (go noc)
        $ \ req -> case evalQuery state req of
            Go next -> go state next
            Lift act next -> do
                res <- lift act 
                either throwME (either throwME (go state)) res
            Throw err -> throwME err

evalQuery :: (Member Query r)
          => A.AcidState NoC -> Query (VE r w) -> 
evalQuery state q = case q of
    IsAdmin uid next
        -> Lift (AA.query' state AQ.IsAdmin) next
