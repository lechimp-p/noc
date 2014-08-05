{-# LANGUAGE TemplateHaskell #-}

module Acidic
where

import Data.Acid (makeAcidic)

import Model

$(makeAcidic ''NoC [])
