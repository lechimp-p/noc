{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.OpMonad
where

import Control.Monad
import Control.Applicative
import Control.Lens (makeLenses)
import Data.Data (Data, Typeable)

import Model.Errors
import Model.BaseTypes
import Model.NoC

data OpContext = OpContext
    { _noc       :: NoC
    , _operator  :: UserId
    }
    deriving (Data, Typeable)

makeLenses ''OpContext

newtype Operation a = Operation { runOperation :: OpContext -> Either Error (OpContext, a) }

runOp' noc oid action = runOperation action $ OpContext noc oid 

instance Monad Operation where
    return v = Operation $ \ s -> Right (s, v)
    m >>= m' = Operation $ \ s ->
        let l = runOperation m s
        in case l of
            (Left e) -> Left e
            (Right (s', v)) ->
                let n = m' v
                in runOperation n s'

instance Functor Operation where
    fmap f v = do
        v' <- v
        return $ f v'

instance Applicative Operation where
    pure = return
    f <*> v = do
        f' <- f
        v' <- v
        return $ f' v'


throw :: Error -> Operation a 
throw e = Operation $ \ s -> Left e

throwOn :: Error -> Bool -> Operation ()
throwOn e c = if c then throw e else return ()
