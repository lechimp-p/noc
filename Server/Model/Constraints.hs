{-# LANGUAGE FlexibleContexts #-}

module Model.Constraints where

import Model.Exec
import Model.Errors
import Model.BaseTypes

import Data.Text (Text)
import Control.Eff

-- ToDo: These need to be implemented correctly somehow...

makeLogin :: Member Exec r => Text -> Eff r Login
makeLogin = return . Login 

makePassword :: Member Exec r => Text -> Eff r Password
makePassword = return . Password 

makeName :: Member Exec r => Text -> Eff r Name
makeName = return . Name 

makeDesc :: Member Exec r => Text -> Eff r Desc
makeDesc = return . Desc 
