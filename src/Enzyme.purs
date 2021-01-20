{-
  @license MIT
  Enzyme.purs
-}

module Enzyme
  ( module Full
  , module Shallow
  , module Wrapper
  , configure
  )
where

import Effect (Effect)
import Enzyme.Full (ReactWrapper, mount, remount) as Full
import Enzyme.Shallow (ShallowWrapper, reshallow, shallow) as Shallow
import Enzyme.Wrapper
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
  ) as Wrapper
import Prelude (Unit)

-- | Configures the Enzyme adapter to provide compatibility with React.
foreign import configure :: Effect Unit
