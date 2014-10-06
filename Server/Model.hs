module Model
    ( module Model.Operations
    , Login
    , mkLogin
    , Password
    , mkPassword
    , Name
    , mkName
    , Desc
    , mkDesc
    , UserId
    , ChanId
    , MsgId
    , Icon (Icon, icnPath)
    , Message
    , Image (Image, imgPath)
    , Notification (..)
    , ChanType (..)
    )
where

import Model.Operations
import Model.Message
import Model.BaseTypes
