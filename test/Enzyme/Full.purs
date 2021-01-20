{-
  @license MIT
  Full.purs
-}

module Test.Enzyme.Full
  ( spec
  )
where

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_, new, read)
import Enzyme (mount, remount, unmount)
import Prelude
import React (ReactElement, ReactThis)
import React (component, createLeafElement) as React
import React.DOM (div') as React
import Test.Enzyme (spec) as Enzyme
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  do
  describe "Mounting"
    do
    it "mounts the React component"
      do
      ref <- liftEffect $ new 0
      let class_ = React.component "Demo" $ spec_ ref
      liftEffect $ void $ mount $ React.createLeafElement class_ { }
      count <- liftEffect $ read ref
      count `shouldEqual` 1
    it "remounts the React component"
      do
      ref <- liftEffect $ new 0
      let class_ = React.component "Demo" $ spec_ ref
      node <- liftEffect $ mount $ React.createLeafElement class_ { }
      liftEffect $ unmount node
      liftEffect $ remount node
      count <- liftEffect $ read ref
      count `shouldEqual` 2
  Enzyme.spec mount
  where
  spec_
    :: Ref Int
    -> ReactThis { } { }
    -> Effect
        { componentDidMount :: Effect Unit
        , state :: { }
        , render :: Effect ReactElement
        }
  spec_ ref this =
    pure
      { componentDidMount : modify_ (_ + 1) ref
      , state : { }
      , render : pure $ React.div' [ ]
      }
