{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Maintenance
import API.Config
import Model
import Model.Acid
import Model.BaseTypes

import Data.Time.Format
import System.Locale
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Web.Routes.Happstack (implSite)
import Happstack.Server (simpleHTTP, nullConf, Conf (..))

initialNoC = mkNoC (Login "admin") (Password "admin") 

accessLog :: FormatTime time
          => String -> String -> time -> String -> Int -> Integer -> String -> String -> IO ()
accessLog host user time request response size referer useragent = do
    putStrLn $ formatTime defaultTimeLocale "%T" time 
               ++ " - " ++ request ++ " : " ++ show response
    hFlush stdout

-- Wait fo sigINT or sigTERM
waitForTermination :: IO ()
waitForTermination = do
    istty <- queryTerminal stdInput
    mv <- newEmptyMVar
    let put = (CatchOnce (putMVar mv ()))
    installHandler softwareTermination put Nothing
    if istty
        then installHandler keyboardSignal put Nothing >> return ()
        else return ()
    takeMVar mv 

main :: IO Int 
main = do
    putStrLn "Starting NoC development server..."
    hFlush stdout
    opts <- readOptions
    mt <- forkIO $ do
        withConfig (optConfigFile opts) $ \ cfg -> do
            withACID (_acidPath cfg) initialNoC $ \acid -> do
                simpleHTTP (nullConf { Happstack.Server.port = cfg ^. serverConfig . API.Config.port
                                     , timeout = cfg ^. serverConfig . threadTimeout
                                     , logAccess = Just accessLog 
                                     }
                           )
                           (runAcidAPISite cfg acid)
        return ()
    waitForTermination
    putStrLn "Terminating NoC development server..."
    hFlush stdout
    killThread mt 
    putStrLn "Done."
    hFlush stdout
    return 0
