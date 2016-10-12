module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall a. Eff (console::CONSOLE|a) Unit
main = do
  log ("Hello, PureScript!")
