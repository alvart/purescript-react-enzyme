# purescript-react-enzyme

[![Build Status](https://travis-ci.com/alvart/purescript-react-enzyme.svg?branch=main)](https://travis-ci.com/alvart/purescript-react-enzyme)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-react-enzyme/badge)](https://pursuit.purescript.org/packages/purescript-react-enzyme)

Purescript bindings for the [Enzyme](https://enzymejs.github.io/enzyme) library.

## Quickstart

### Setting up the test environment before running any test

Before you write any unit tests with Enzyme, make sure to configure the [Enzyme Adapter](https://www.npmjs.com/package/enzyme-adapter-react-16) and the [Global DOM](https://www.npmjs.com/package/global-jsdom) **if** you are planning to use Full wrappers as opposed to Shallow ones.

Main.purs
```purescript
import Effect (Effect)
import Effect.Aff (launchAff_)
import Enzyme (configure, withGlobalDOM)
import Prelude
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  do
  configure
  withGlobalDOM
  launchAff_ $ runSpec [ consoleReporter ]
    do
    calculatorSpec
    todoSpec
    enzymeSpec
```

### Writing unit tests

Enzyme.purs
```purescript
import Effect.Class (liftEffect)
import Enzyme (children, length, mount)
import Prelude
import React.DOM (button', div', span') as React
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  do
  describe "Traversing"
    do
    it "counts the number of children nodes"
      do
      node <-
        liftEffect $ mount $ React.div' [ React.span' [ ], React.button' [ ] ]
      count <- liftEffect $ children node >>= length
      count `shouldEqual` 2
```

### More examples

There are examples for every function in this library inside the [test folder](test) of this repository.
