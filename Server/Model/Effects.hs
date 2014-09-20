{-# LANGUAGE FlexibleContexts #-}

module Model.Effects where

import Control.Eff

import Model.BaseTypes
import Model.Query
import Model.Update

-- Queries

isAdmin :: Member Query r => UserId -> Eff r Bool
isAdmin uid = send $ \ next -> inj (IsAdmin uid next)

getChanName :: Member Query r => ChanId -> Eff r Name
getChanName cid = send $ \ next -> inj (ChanQuery cid (GetChanName next))

getChanDesc :: Member Query r => ChanId -> Eff r Desc 
getChanDesc cid = send $ \ next -> inj (ChanQuery cid (GetChanDesc next))

getChanType :: Member Query r => ChanId -> Eff r ChanType 
getChanType cid = send $ \ next -> inj (ChanQuery cid (GetChanType next))

getChanImage :: Member Query r => ChanId -> Eff r (Maybe Image)
getChanImage cid = send $ \ next -> inj (ChanQuery cid (GetChanImage next))

-- Updates

addAdmin :: Member Update r => UserId -> Eff r () 
addAdmin uid = send $ \ next -> inj (AddAdmin uid next)
