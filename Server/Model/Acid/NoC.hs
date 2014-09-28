{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Acid.NoC 
    ( module Model.Acid.NoC
    , module Model.Simple.NoC
    )
where

import Model.Simple.NoC

$(deriveSafeCopy 0 'base ''NoC)
