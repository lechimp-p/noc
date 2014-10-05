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
    , ChanType
    , MsgId
    , Icon (Icon, icnPath)
    , Message
    , Image (Image, imgPath)
    , Notification (..)
    )
where

import Model.Operations
import Model.Message
import Model.BaseTypes
