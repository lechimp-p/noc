{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Effects where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L 
import Control.Eff

data API session config n 
    = GetSession                (session -> n)     
    | PutSession session        (() -> n)
    | ExpireSession             (() -> n)
    | Respond Int L.ByteString  (() -> n) 
    | LookGet String            (String -> n)
    | Timestamp                 (UTCTime -> n)
    | Config                    (config -> n)
    | GetBody                   (L.ByteString -> n)               
    | forall a. Abort           (a -> n)
    deriving (Typeable)

instance Functor (API session config) where
    fmap f (GetSession n)       = GetSession (f . n)
    fmap f (PutSession s n)     = PutSession s (f . n)
    fmap f (ExpireSession n)    = ExpireSession (f . n)
    fmap f (Respond s t n)      = Respond s t (f . n)
    fmap f (LookGet t n)        = LookGet t (f . n)
    fmap f (Timestamp n)        = Timestamp (f . n)
    fmap f (Config n)           = Config (f . n)
    fmap f (GetBody n)          = GetBody (f . n)
    fmap f (Abort n)            = Abort (f . n)

----------------
-- Basic Effects
----------------

getSession :: forall session config r.
              ( Typeable session
              , Typeable config
              , Member (API session config) r
              ) 
           => Eff r session 
getSession = send inj'
    where
    inj' :: forall w. (session -> VE r w) -> Union r (VE r w)
    inj' next = inj (GetSession next :: API session config (VE r w))


putSession :: forall session config r.
              ( Typeable session
              , Typeable config
              , Member (API session config) r
              ) 
           => session -> Eff r ()
putSession sess = send inj'
    where
    inj' :: forall w. (() -> VE r w) -> Union r (VE r w)
    inj' next = inj (PutSession sess next :: API session config (VE r w))


expireSession :: forall session config r.
                 ( Typeable session
                 , Typeable config
                 , Member (API session config) r
                 )
              => Eff r ()
expireSession = send inj'
    where
    inj' :: forall w. (()-> VE r w) -> Union r (VE r w)
    inj' next = inj (ExpireSession next :: API session config (VE r w))


respond :: forall session config r.
              ( Typeable session
              , Typeable config
              , Member (API session config) r
              )
           => Int -> L.ByteString -> Eff r () 
respond status resp = send inj'
    where
    inj' :: forall w. (() -> VE r w) -> Union r (VE r w)
    inj' next = inj (Respond status resp next :: API session config (VE r w))


lookGet :: forall session config r.
           ( Typeable session
           , Typeable config
           , Member (API session config) r
           )
        => String -> Eff r String
lookGet var = send inj'
    where
    inj' :: forall w. (String -> VE r w) -> Union r (VE r w)
    inj' next = inj (LookGet var next :: API session config (VE r w))


timestamp :: forall session config r.
             ( Typeable session
             , Typeable config
             , Member (API session config) r
             )
          => Eff r UTCTime
timestamp = send inj'
    where
    inj' :: forall w. (UTCTime -> VE r w) -> Union r (VE r w)
    inj' next = inj (Timestamp next :: API session config (VE r w))


config :: forall session config r.
          ( Typeable session
          , Typeable config
          , Member (API session config) r
          )
       => Eff r config
config = send inj'
    where
    inj' :: forall w. (config -> VE r w) -> Union r (VE r w)
    inj' next = inj (Config next :: API session config (VE r w))


getBody :: forall session config r.
           ( Typeable session
           , Typeable config
           , Member (API session config) r
           )
        => Eff r L.ByteString 
getBody = send inj'
    where
    inj' :: forall w. (L.ByteString -> VE r w) -> Union r (VE r w)
    inj' next = inj (GetBody next :: API session config (VE r w))

abort :: forall session config r a.
         ( Typeable session
         , Typeable config
         , Member (API session config) r
         )
      => Eff r a 
abort = send inj'
    where
    inj' :: forall w. (a -> VE r w) -> Union r (VE r w)
    inj' next = inj (Abort next :: API session config (VE r w))
