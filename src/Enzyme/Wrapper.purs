{-
  @license MIT
  Wrapper.purs
-}

module Enzyme.Wrapper
  ( class Wrapper
  , at
  , childAt
  , children
  , debug
  , enumerate
  , every
  , everyWhere
  , find
  , findWhere
  , first
  , forEach
  , get
  , hostNodes
  , invoke
  , is
  , isEmpty
  , last
  , length
  , matches
  , name
  , parent
  , parents
  , property
  , reduce
  , setProperty
  , skip
  , slice
  , some
  , someWhere
  , text
  , unmount
  )
where

import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Prelude
import React (ReactElement)

-- | The `Wrapper` type class represents any Enzyme wrapper of React components
-- | with an interface to manipulate or traverse them.
class Wrapper w
  where
  -- | Returns a wrapper around the node at a given index of the current
  -- | wrapper.
  at :: Int -> w -> Effect w

  -- | Returns a new wrapper with all of the children of the node(s) in the
  -- | current wrapper.
  children :: w -> Effect w

  -- | Returns an HTML-like string of the wrapper for debugging purposes. Useful
  -- | to print out to the console when tests are not passing when you expect
  -- | them to.
  debug :: { ignoreProps :: Boolean, verbose :: Boolean } -> w -> Effect String

  -- | Finds every node in the render tree that returns true for the provided
  -- | predicate function.
  findWhere :: (w -> Effect Boolean) -> w -> Effect w

  -- | Maps the current array of nodes to another array. Each node is passed in
  -- | as a wrapper to the map function.
  forEach :: (w -> Int -> Effect Unit) -> w -> Effect Unit

  -- | Returns the node at a given index of the current wrapper.
  get :: Int -> w -> Effect ReactElement

  -- | Returns a new wrapper with only host nodes. When using React-dom, host
  -- | nodes are HTML elements rather than custom React components, e.g. `<div>`
  -- | versus `<MyComponent>`.
  hostNodes :: w -> Effect w

  -- | Invokes a property as a function with an Array of arguments.
  invoke :: String -> w -> Array Foreign -> Effect Unit

  -- | Returns whether or not the single wrapped node matches the provided
  -- | selector. It must be a single-node wrapper.
  is :: String -> w -> Effect Boolean

  -- | Returns the number of nodes inside the current wrapper.
  length :: w -> Effect Int

  -- | Returns whether or not a patternNode React element matches any element in
  -- | the render tree. It must be a single-node wrapper, and only the root node
  -- | is checked.
  matches :: ReactElement -> w -> Effect Boolean

  -- | Returns the name of the current node of this wrapper. If it's a composite
  -- | component, this will be the name of the component. If it's a native DOM
  -- | it will be a string of the tag name.
  name :: w -> Effect String

  -- | Returns a wrapper around all of the parents/ancestors of the single node
  -- | in the wrapper. Does not include the node itself.
  parents :: w -> Effect w

  -- | Returns the prop value for the root node of the wrapper with the provided
  -- | key. It must be a single-node wrapper.
  property :: String -> w -> Effect Foreign

  -- | Applies the provided reducing function to every node in the wrapper to
  -- | reduce to a single value. Each node is passed in as a wrapper, and is
  -- | processed from left to right.
  reduce :: forall a . (a -> w -> Int -> Effect a) -> a -> w -> Effect a

  -- | A method that sets the property of the root component, and re-renders.
  -- | Useful for when you are wanting to test how the component behaves over
  -- | time with changing props. Blocks until the operation completes.
  setProperty :: String -> Foreign -> w -> Aff Unit

  -- | Returns a new wrapper with a subset of the nodes of the original wrapper,
  -- | according to the rules of `Array#slice`.
  slice :: Int -> Int -> w -> Effect w

  -- | Returns a string of the rendered text of the current render tree.
  text :: w -> Effect String

  -- | A method that unmounts the component. This can be used to simulate a
  -- | component going through an unmount/mount lifecycle.
  unmount :: w -> Effect Unit

-- | Returns a new wrapper with child at the specified index.
childAt :: forall w . Wrapper w => Int -> w -> Effect w
childAt index node = children node >>= at index

-- | Enumerates all the nodes in the current wrapper and their children.
enumerate :: forall w . Wrapper w => w -> Effect w
enumerate = findWhere (const $ pure true)

-- | Returns whether or not all of the nodes in the wrapper match the provided
-- | selector.
every :: forall w . Wrapper w => String -> w -> Effect Boolean
every selector = everyWhere (is selector)

-- | Returns whether or not all of the nodes in the wrapper pass the provided
-- | predicate function.
everyWhere
  :: forall w . Wrapper w => (w -> Effect Boolean) -> w -> Effect Boolean
everyWhere predicate node =
  do
  let soFar match node_ _ = map (match && _) $ predicate node_
  enumerate node >>= reduce soFar true

-- | Finds every node in the render tree of the current wrapper that matches
-- | the provided selector.
find :: forall w . Wrapper w => String -> w -> Effect w
find selector = findWhere (is selector)

-- | Reduce the set of matched nodes to the first in the set, just like `at 0`.
first :: forall w . Wrapper w => w -> Effect w
first = at 0

-- | Returns whether or not the wrapper is empty.
isEmpty :: forall w . Wrapper w => w -> Effect Boolean
isEmpty node =
  do
  count <- length node
  pure $ count == 0

-- | Reduce the set of matched nodes to the last in the set.
last :: forall w . Wrapper w => w -> Effect w
last node =
  do
  index <- length node
  at (index - 1) node

-- | Returns a wrapper with the direct parent of the node in the current
-- | wrapper.
parent :: forall w . Wrapper w => w -> Effect w
parent node = parents node >>= first

-- | Returns a new wrapper with a subset of the nodes of the original wrapper
-- | after skipping the first `n` nodes.
skip :: forall w . Wrapper w => Int -> w -> Effect w
skip count node =
  do
  size <- length node
  slice count size node

-- | Returns whether or not any of the nodes in the wrapper match the provided
-- | selector.
some :: forall w . Wrapper w => String -> w -> Effect Boolean
some selector = someWhere (is selector)

-- | Returns whether or not any of the nodes in the wrapper pass the provided
-- | predicate function.
someWhere
  :: forall w . Wrapper w => (w -> Effect Boolean) -> w -> Effect Boolean
someWhere predicate node =
  do
  let soFar match node_ _ = map (match || _) $ predicate node_
  enumerate node >>= reduce soFar false
