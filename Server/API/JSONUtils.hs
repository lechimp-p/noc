{-# LANGUAGE GADTs #-}

module API.JSONUtils
where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Types
import Data.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Lens
import Control.Lens.Prism

{--
parseBody :: (Monad m, MonadIO m) 
          => (Object -> Parser (APIMonadT url session m a)) -> APIMonadT url session m a 
parseBody parser = do
    body <- getBody
    let maybeAction = decode body >>= parseMaybe parser
    case maybeAction of
        Just action -> action
        Nothing -> badRequest' "Could not decode body."
--}

data JSONError'
    = MissingProp Text
    | CantDecodeObject
    deriving (Show)

data JSONMonadT m a where
    Get :: (FromJSON a, Monad m) => Text -> JSONMonadT m (Maybe a)
    Set :: (ToJSON a, Monad m) => Text -> a -> JSONMonadT m ()
    Throw :: Monad m => JSONError' -> JSONMonadT m a 
    Return :: Monad m => a -> JSONMonadT m a
    Bind :: Monad m => JSONMonadT m a -> (a -> JSONMonadT m b) -> JSONMonadT m b
    Lift :: Monad m => m a -> JSONMonadT m a

instance Monad m => Monad (JSONMonadT m) where
    return = Return
    (>>=) = Bind

instance MonadTrans JSONMonadT where
    lift = Lift

runJSONMonadT :: (Monad m) 
              => JSONMonadT m a -> Object -> m (Either JSONError' (a, Value))
runJSONMonadT m obj = runJSONMonadT' m obj [] >>= return . over (_Right . _2) object

runJSONMonadT' :: (Monad m)
               => JSONMonadT m a -> Object -> [Pair] -> m (Either JSONError' (a, [Pair]))
runJSONMonadT' m obj ps =
    case m of
        Set prop val    -> return $ Right ((), (prop, toJSON val) : ps)
        Throw err       -> return $ Left err
        Return val      -> return $ Right (val, ps)
        Lift m          -> m >>= \ v -> return $ Right (v, ps)
        Get prop        -> 
            let value = HM.lookup prop obj  -- Maybe Value
                result = fmap (parseMaybe parseJSON) value -- Maybe Maybe a
            in return $ Right (join result, ps)
        Bind v f        -> do
            v' <- runJSONMonadT' v obj ps 
            case v' of
                Left err -> return $ Left err
                Right (res, ps') -> runJSONMonadT' (f res) obj (ps' ++ ps)

maybeProp :: (FromJSON a, Monad m) => Text -> JSONMonadT m (Maybe a)
maybeProp = Get

prop :: (FromJSON a, Monad m) => Text -> JSONMonadT m a
prop n = do 
    p <- maybeProp n
    case p of
        Nothing -> Throw $ MissingProp n
        Just p' -> return p'

infixl 9 <:
infixl 9 <::

(<:) :: (ToJSON a, Monad m) => Text -> a -> JSONMonadT m ()
(<:) = Set

(<::) :: (ToJSON a, Monad m) => Text -> JSONMonadT m a -> JSONMonadT m ()
n <:: m = do
    val <- m
    Set n val
