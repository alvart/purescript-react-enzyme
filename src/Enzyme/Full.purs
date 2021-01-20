{-
  @license MIT
  Full.purs
-}

module Enzyme.Full
  ( ReactWrapper
  , mount
  , remount
  )
where

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , EffectFn4
  , mkEffectFn1
  , mkEffectFn2
  , mkEffectFn3
  , runEffectFn1
  , runEffectFn2
  , runEffectFn3
  , runEffectFn4
  )
import Enzyme.Wrapper (class Wrapper)
import Foreign (Foreign)
import Prelude
import React (ReactElement)

-- | Represents an Enzyme wrapper for components that may interact with DOM APIs
-- | or that are part of higher order components.
foreign import data ReactWrapper :: Type

-- ReactWrapper :: Wrapper

instance wrapperReactWrapper :: Wrapper ReactWrapper
  where
  at = runEffectFn2 _at

  children = runEffectFn1 _children

  debug = runEffectFn2 _debug

  findWhere fn = runEffectFn2 _findWhere (mkEffectFn1 fn)

  forEach fn = runEffectFn2 _forEach (mkEffectFn2 fn)

  get = runEffectFn2 _get

  hostNodes = runEffectFn1 _hostNodes

  invoke fnName node arguments =
    do
    fn <- runEffectFn2 _invoke fnName node
    runEffectFn1 fn arguments

  is = runEffectFn2 _is

  length = runEffectFn1 _length

  matches = runEffectFn2 _matches

  name = runEffectFn1 _name

  parents = runEffectFn1 _parents

  property = runEffectFn2 _property

  reduce fn = runEffectFn3 _reduce (mkEffectFn3 fn)

  setProperty name value node =
    makeAff \cb ->
      do
      runEffectFn4 _setProperty name value (cb $ Right unit) node
      pure nonCanceler

  slice = runEffectFn3 _slice

  text = runEffectFn1 _text

  unmount = runEffectFn1 _unmount

-- | Creates a `ReactWrapper` from a React element.
mount :: ReactElement -> Effect ReactWrapper
mount = runEffectFn1 _mount

-- | A method that re-mounts the component, if it is not currently mounted. This
-- | can be used to simulate a component going through an unmount/mount
-- | lifecycle.
remount :: ReactWrapper -> Effect Unit
remount = runEffectFn1 _remount

-- Returns a wrapper around the node at a given index of the current wrapper.
foreign import _at :: EffectFn2 Int ReactWrapper ReactWrapper

-- Returns a new wrapper with all of the children of the node(s) in the current
-- wrapper.
foreign import _children :: EffectFn1 ReactWrapper ReactWrapper

-- Returns an HTML-like string of the wrapper for debugging purposes. Useful to
-- print out to the console when tests are not passing when you expect them to.
foreign import _debug
  :: EffectFn2
      { ignoreProps :: Boolean, verbose :: Boolean } ReactWrapper String

-- Finds every node in the render tree that returns true for the provided
-- predicate function.
foreign import _findWhere
  :: EffectFn2 (EffectFn1 ReactWrapper Boolean) ReactWrapper ReactWrapper

-- Maps the current array of nodes to another array. Each node is passed in as a
-- `ReactWrapper` to the map function.
foreign import _forEach
  :: EffectFn2 (EffectFn2 ReactWrapper Int Unit) ReactWrapper Unit

-- Returns the node at a given index of the current wrapper.
foreign import _get :: EffectFn2 Int ReactWrapper ReactElement

-- Returns a new wrapper with only host nodes. When using React-dom, host nodes
-- are HTML elements rather than custom React components, e.g. `<div>` versus
-- `<MyComponent>`.
foreign import _hostNodes :: EffectFn1 ReactWrapper ReactWrapper

-- Invokes a property as a function with an Array of arguments.
foreign import _invoke
  :: EffectFn2 String ReactWrapper (EffectFn1 (Array Foreign) Unit)

-- Returns whether or not the single wrapped node matches the provided selector.
-- It must be a single-node wrapper.
foreign import _is :: EffectFn2 String ReactWrapper Boolean

-- Returns the number of nodes inside the current wrapper.
foreign import _length :: EffectFn1 ReactWrapper Int

-- Returns whether or not a patternNode React element matches any element in the
-- render tree. It must be a single-node wrapper, and only the root node is
-- checked.
foreign import _matches :: EffectFn2 ReactElement ReactWrapper Boolean

-- Creates a `ReactWrapper` from a React element.
foreign import _mount :: EffectFn1 ReactElement ReactWrapper

-- Returns the name of the current node of this wrapper. If it's a composite
-- component, this will be the name of the component. If it's a native DOM node,
-- it will be a string of the tag name.
foreign import _name :: EffectFn1 ReactWrapper String

-- Returns a wrapper around all of the parents/ancestors of the single node in
-- the wrapper. Does not include the node itself.
foreign import _parents :: EffectFn1 ReactWrapper ReactWrapper

-- Returns the prop value for the root node of the wrapper with the provided
-- key. It must be a single-node wrapper.
foreign import _property :: EffectFn2 String ReactWrapper Foreign

-- Applies the provided reducing function to every node in the wrapper to reduce
-- to a single value. Each node is passed in as a `ReactWrapper`, and is
-- processed from left to right.
foreign import _reduce
  :: forall a . EffectFn3 (EffectFn3 a ReactWrapper Int a) a ReactWrapper a

-- A method that re-mounts the component, if it is not currently mounted. This
-- can be used to simulate a component going through an unmount/mount lifecycle.
foreign import _remount :: EffectFn1 ReactWrapper Unit

-- A method that sets the property of the root component, and re-renders. Useful
-- for when you are wanting to test how the component behaves over time with
-- changing props.
foreign import _setProperty
  :: EffectFn4 String Foreign (Effect Unit) ReactWrapper Unit

-- Returns a new wrapper with a subset of the nodes of the original wrapper,
-- according to the rules of `Array#slice`.
foreign import _slice :: EffectFn3 Int Int ReactWrapper ReactWrapper

-- Returns a string of the rendered text of the current render tree.
foreign import _text :: EffectFn1 ReactWrapper String

-- A method that unmounts the component. This can be used to simulate a
-- component going through an unmount/mount lifecycle.
foreign import _unmount :: EffectFn1 ReactWrapper Unit
