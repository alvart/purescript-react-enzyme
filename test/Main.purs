{-
  @license MIT
  Main.purs
-}

module Test.Main
  ( main
  )
where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Enzyme (configure)
import Prelude
import Test.Enzyme.Full (spec) as Full
import Test.Enzyme.Shallow (spec) as Shallow
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  do
  configure
  registerGlobalDOM
  launchAff_ $ runSpec [ consoleReporter ]
    do
    describe "Full Wrapper" Full.spec
    describe "Shallow Wrapper" Shallow.spec

foreign import registerGlobalDOM :: Effect Unit
