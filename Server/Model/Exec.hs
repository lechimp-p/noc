{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Model.Exec where

import Data.Text
import Data.Data (Typeable)
import Control.Eff

import Model.BaseTypes
import Model.Errors

data Exec n
    = GetOperatorId             (Maybe UserId -> n)
    | forall a. ThrowME Error   (a -> n)
    | DoLogin Login Password    (UserId -> n)
    | DoLogout                  (() -> n) 
    deriving (Typeable)

instance Functor Exec where
    fmap f (GetOperatorId c)    = GetOperatorId (f . c)
    fmap f (DoLogin l pw c)     = DoLogin l pw (f . c)
    fmap f (DoLogout c)         = DoLogout (f . c)
    fmap f (ThrowME err _)      = ThrowME err (error "Model.Exec (Functor Exec): fmap for ThrowME not implemented. The continuation should not be called.")

getOperatorId :: Member Exec r => Eff r (Maybe UserId) 
getOperatorId = send $ \ next -> inj (GetOperatorId next)

throwME :: Member Exec r => Error -> Eff r a 
throwME err = send $ \ next -> inj (ThrowME err next) 

doLogin :: Member Exec r => Login -> Password -> Eff r UserId
doLogin l pw = send $ \ next -> inj (DoLogin l pw next)

doLogout :: Member Exec r => Eff r ()
doLogout = send $ \ next -> inj (DoLogout next)
