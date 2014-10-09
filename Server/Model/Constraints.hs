module Model.Constraints where

import Model.Exec
import Model.Errors

import Control.Eff

makeLogin :: Member Exec r => Text -> Eff r Login
make Login = undefined

makePassword :: Member Exec r => Text -> Eff r Password
