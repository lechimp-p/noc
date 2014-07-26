module Model.OpMonad
where

import Control.Monad
import Control.Applicative

import Model.BaseTypes
import Model.NoC

data OpContext = OpContext
    { noc       :: NoC
    , operator  :: UserId
    }

newtype Operation e a = Operation { runOperation :: OpContext -> Either e (OpContext, a) }

runOp noc oid action = runOperation action $ OpContext noc oid 

instance Monad (Operation e) where
    return v = Operation $ \ s -> Right (s, v)
    m >>= m' = Operation $ \ s ->
        let l = runOperation m s
        in case l of
            (Left e) -> Left e
            (Right (s', v)) ->
                let n = m' v
                in runOperation n s

instance Functor (Operation e) where
    fmap f v = do
        v' <- v
        return $ f v'

instance Applicative (Operation e) where
    pure = return
    f <*> v = do
        f' <- f
        v' <- v
        return $ f' v'
