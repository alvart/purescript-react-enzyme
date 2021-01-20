{-
  @license MIT
  Shallow.purs
-}

module Test.Enzyme.Shallow
  ( spec
  )
where

import Effect (Effect)
import Effect.Class (liftEffect)
import Enzyme (debug, find, reshallow, shallow)
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
    it "remounts the React component"
      do
      let element = React.div' [ component "Child" spec_ ]
      node <- liftEffect $ shallow element >>= find "Child" >>= reshallow
      html <- liftEffect $ debug { ignoreProps : false, verbose : false } node
      html `shouldEqual` "<div />"
  Enzyme.spec shallow
  where
  component name = flip React.createLeafElement { } <<< React.component name

  spec_
    :: ReactThis { } {  }
    -> Effect { state :: { }, render :: Effect ReactElement }
  spec_ this = pure { state : { } , render : pure $ React.div' [ ] }
