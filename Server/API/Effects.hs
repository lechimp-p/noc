{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module API.Effects where

import API.Config
import API.Session

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Data (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Aeson (Value, encode)
import Control.Eff

class IsResponse r where
    contentType :: r -> B.ByteString
    content :: r -> L.ByteString

data HTTPMethod
    = GET
    | POST
    | PUT
    | DELETE
    | HEAD
    deriving (Typeable)
     
data API n 
    = GetSession                                (AuthData -> n)     
    | PutSession AuthData                       (() -> n)
    | ExpireSession                             (() -> n)
    | forall r. IsResponse r => Respond Int r   (r -> n) 
    | LookGet String                            (String -> n)
    | Timestamp                                 (UTCTime -> n)
    | forall a. Config (APIConfig -> a)         (a -> n)
    | GetBody                                   (L.ByteString -> n)               
    | Method                                    (HTTPMethod -> n)                         
    -- | forall a. Abort                           (a -> n)
    | WriteFile FilePath B.ByteString           (() -> n)
    | RemoveFile FilePath                       (Bool -> n)
    deriving (Typeable)

instance Functor API where
    fmap f (GetSession n)       = GetSession (f . n)
    fmap f (PutSession s n)     = PutSession s (f . n)
    fmap f (ExpireSession n)    = ExpireSession (f . n)
    fmap f (Respond s t n)      = Respond s t (f . n)
    fmap f (LookGet t n)        = LookGet t (f . n)
    fmap f (Timestamp n)        = Timestamp (f . n)
    fmap f (Config f' n)        = Config f' (f . n)
    fmap f (GetBody n)          = GetBody (f . n)
    -- fmap f (Abort n)            = Abort (f . n)
    fmap f (WriteFile p c n)    = WriteFile p c (f . n)
    fmap f (RemoveFile p n)     = RemoveFile p (f . n)

----------------
-- Basic Effects
----------------

getSession :: Member API r 
           => Eff r AuthData 
getSession = send $ \ next -> inj (GetSession next) 


putSession :: Member API r
           => AuthData -> Eff r ()
putSession sess = send $ \ next -> inj (PutSession sess next)


expireSession :: Member API r
              => Eff r ()
expireSession = send $ \ next -> inj (ExpireSession next)


respond :: ( Member API r
           , IsResponse resp 
           )
           => Int -> resp -> Eff r resp
respond status resp = send $ \ next -> inj (Respond status resp next)


lookGet :: Member API r
        => String -> Eff r String
lookGet var = send $ \ next -> inj (LookGet var next)


timestamp :: Member API r
          => Eff r UTCTime
timestamp = send $ \ next -> inj (Timestamp next)


config :: Member API r
       => (APIConfig -> a) -> Eff r a 
config f = send $ \ next -> inj (Config f next)


getBody :: Member API r 
        => Eff r L.ByteString 
getBody = send $ \ next -> inj (GetBody next)


method :: Member API r
       => Eff r HTTPMethod
method = send $ \ next -> inj (Method next)


--abort :: Member API r
--      => Eff r a 
--abort = send $ \ next -> inj (Abort next)


writeFile :: Member API r
          => FilePath -> B.ByteString -> Eff r ()
writeFile p c = send $ \ next -> inj (WriteFile p c next)

removeFile :: Member API r
           => FilePath -> Eff r Bool
removeFile p = send $ \ next -> inj (RemoveFile p next)

----------------
-- Derived Stuff 
----------------

ok :: ( Member API r
      , IsResponse resp
      )
   => resp -> Eff r () 
ok = respond 200

-- TODO: status code??
noContent :: Member API r => Eff r r 
noContent = respond 201 ("" :: Text)

-- TODO: status code??
badRequest :: Member API r => Text -> Eff r r 
badRequest = respond 300 . L.pack . T.unpack

instance IsResponse L.ByteString where
    content = id
    contentType = const "text/plain"

instance IsResponse Text where
    content = L.pack . T.unpack
    contentType = const "text/plain"

instance IsResponse Value where
    content = encode
    contentType = const "application/json"

instance IsResponse (Maybe Value) where
    content Nothing = ""
    content (Just v) = encode v
    contentType Nothing = ""
    contentType _ = "application/json"
    
