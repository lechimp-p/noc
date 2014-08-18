{-# LANGUAGE GADTs #-}

module API.JSONUtils
    ( MonadJSONError
    , throwJSONError
    , MonadJSON 
    , readProp
    , writeProp
    , maybeProp
    , prop
    , (<:)
    , (<:.) 
    , (.:>)
    , (?:>)
    , JSONError
    , JSONMonadT
    , runJSONMonadT
    , runJSONMonadT'
    , getObject
    , getPairs
    , setPairs
    )
where

import Data.ByteString.Lazy
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Types
import Data.Text
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Prism

data JSONError
    = MissingProp Text
    | CantDecodeObject
    deriving (Show)

class Monad m => MonadJSONError m where
    throwJSONError :: JSONError -> m a

class Monad m => MonadJSON m where
    readProp  :: FromJSON a => Text -> m (Maybe a)
    writeProp :: ToJSON a => Text -> a -> m () 

infixl 9 <:
infixl 9 <:.
infixl 9 .:>
infixl 9 ?:> 

(<:) :: (ToJSON a, MonadJSON m) => Text -> a -> m a 
s <: a = writeProp s a >> return a

(<:.) :: (ToJSON a, MonadJSON m) => Text -> m a -> m a 
n <:. m = do
    val <- m
    n <: val

(.:>) :: (FromJSON a, MonadJSON m, MonadJSONError m) => Text -> (a -> m b) -> m b
n .:> m = do
    p <- prop n
    m p

(?:>) :: (FromJSON a, MonadJSON m) => Text -> (a -> m b) -> m (Maybe b)
n ?:> m = do
    p' <- maybeProp n
    case p' of
        Nothing -> return Nothing
        Just p  -> do
            res <- m p
            return (Just res) 

maybeProp :: (FromJSON a, MonadJSON m) => Text -> m (Maybe a)
maybeProp = readProp 

prop :: (FromJSON a, MonadJSON m, MonadJSONError m) => Text -> m a
prop n = do 
    p <- maybeProp n
    case p of
        Nothing -> throwJSONError $ MissingProp n
        Just p' -> return p'

getObject :: Monad m => JSONMonadT m Object
getObject = GetObject
getPairs :: Monad m => JSONMonadT m [Pair] 
getPairs = GetPairs
setPairs :: Monad m => [Pair] -> JSONMonadT m ()
setPairs = SetPairs

data JSONMonadT m a where
    ReadProp :: (FromJSON a, Monad m) => Text -> JSONMonadT m (Maybe a)
    WriteProp :: (ToJSON a, Monad m) => Text -> a -> JSONMonadT m ()
    Return :: Monad m => a -> JSONMonadT m a
    Bind :: Monad m => JSONMonadT m a -> (a -> JSONMonadT m b) -> JSONMonadT m b
    Lift :: Monad m => m a -> JSONMonadT m a
    Plus :: MonadPlus m => JSONMonadT m a -> JSONMonadT m a -> JSONMonadT m a
    GetObject :: Monad m => JSONMonadT m Object
    GetPairs :: Monad m => JSONMonadT m [Pair]
    SetPairs :: Monad m => [Pair] -> JSONMonadT m ()

instance Monad m => Monad (JSONMonadT m) where
    return = Return
    (>>=) = Bind

instance MonadTrans JSONMonadT where
    lift = Lift

instance MonadJSONError m => MonadJSONError (JSONMonadT m) where
    throwJSONError = lift . throwJSONError

instance Monad m => MonadJSON (JSONMonadT m) where
    readProp = ReadProp
    writeProp = WriteProp

instance MonadIO m => MonadIO (JSONMonadT m) where
    liftIO = lift . liftIO

instance Monad m => Functor (JSONMonadT m) where
    fmap f m = m >>= return . f 

instance Monad m => Applicative (JSONMonadT m) where
    pure = return
    m <*> m' = m >>= \f -> m' >>= return . f

instance MonadPlus m => MonadPlus (JSONMonadT m) where
    mzero = lift mzero
    mplus = Plus
 
runJSONMonadT :: MonadJSONError m 
              => JSONMonadT m a -> ByteString -> m (a, Value)
runJSONMonadT m bs = 
    case decode bs of
        Nothing -> throwJSONError CantDecodeObject
        Just obj -> runJSONMonadT' m obj [] >>= return . over _2 object

runJSONMonadT' :: (MonadJSONError m)
               => JSONMonadT m a -> Object -> [Pair] -> m (a, [Pair])
runJSONMonadT' m obj ps =
    case m of
        WriteProp p v   -> return ((), (p, toJSON v) : ps)
        Return val      -> return (val, ps)
        Lift m          -> m >>= \ v -> return (v, ps)
        ReadProp prop   -> 
            let value = HM.lookup prop obj  -- Maybe Value
                result = fmap (parseMaybe parseJSON) value -- Maybe Maybe a
            in return $ (join result, ps)
        Bind v f        -> do
            (v, ps') <- runJSONMonadT' v obj ps 
            runJSONMonadT' (f v) obj (ps' ++ ps)
        Plus l r        -> (runJSONMonadT' l obj ps) `mplus` (runJSONMonadT' r obj ps)
        GetObject       -> return (obj, ps)
        GetPairs        -> return (ps, ps)
        SetPairs ps'    -> return ((), ps')
