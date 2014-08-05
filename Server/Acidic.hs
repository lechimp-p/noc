{-# LANGUAGE TemplateHaskell #-}

module ACID
where

import Data.Acid (makeAcidic)

import Model.NoC (NoC)

$(makeAcidic ''NoC [])
