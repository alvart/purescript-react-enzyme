{-
  @license MIT
  Enzyme.purs
-}

module Test.Enzyme
  ( spec
  )
where

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.String (trim)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref, modify_, new, read)
import Enzyme
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
import Foreign (readString, unsafeToForeign)
import Prelude
import React (ReactElement, ReactThis)
import React (component, createLeafElement) as React
import React.DOM (button, button', div, div', span, span', text) as React
import React.DOM.Props (className, onClick, style) as React
import React.SyntheticEvent (clientX)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall w . Wrapper w => (ReactElement -> Effect w) -> Spec Unit
spec wrapper =
  do
  describe "Traversing"
    do
    it "retrieves the first node"
      do
      node <- wrapper_ $ React.div' [ React.span' [ ], React.button' [ ] ]
      tagName <- liftEffect $ children node >>= first >>= name
      tagName `shouldEqual` "span"
    it "retrieves the last node"
      do
      node <- wrapper_ $ React.div' [ React.span' [ ], React.button' [ ] ]
      tagName <- liftEffect $ children node >>= last >>= name
      tagName `shouldEqual` "button"
    it "retrieves the nth node"
      do
      node <-
        wrapper_
          $ React.div' [ React.div' [ ], React.span' [ ], React.button' [ ] ]
      tagName <- liftEffect $ children node >>= at 1 >>= name
      tagName `shouldEqual` "span"
    it "retrieves the children nodes"
      do
      node <- wrapper_ $ React.div' [ React.span' [ ], React.button' [ ] ]
      html <- liftEffect $ children node >>= debug_
      html `shouldEqual` "<span /> <button />"
    it "retrieves the first parent node"
      do
      node <- wrapper_ $ React.div' [ React.span' [ React.button' [ ] ] ]
      child <- liftEffect $ childAt 0 node >>= childAt 0
      tagName <- liftEffect $ parent child >>= name
      tagName `shouldEqual` "span"
    it "retrieves the parent nodes"
      do
      node <- wrapper_ $ React.div' [ React.span' [ React.button' [ ] ] ]
      child <- liftEffect $ childAt 0 node >>= childAt 0
      parents <- liftEffect $ parents child
      parentTagName <- liftEffect $ first parents >>= name
      parentTagName `shouldEqual` "span"
      grandParentTagName <- liftEffect $ at 1 parents >>= name
      grandParentTagName `shouldEqual` "div"
    it "retrieves the nth React element"
      do
      node <-
        wrapper_
          $ React.div' [ React.span' [ ], React.button' [ ], React.span' [ ] ]
      html <- liftEffect $ children node >>= get 1 >>= wrapper >>= debug_
      html `shouldEqual` "<button />"
    it "counts each node"
      do
      node <-
        wrapper_ $ React.div' [ React.div' [ ], React.div' [ ], React.div' [ ] ]
      count <- liftEffect $ children node >>= length
      count `shouldEqual` 3
    it "enumerates every node"
      do
      node <-
        wrapper_
          $ React.div' [ React.span' [ ], React.button' [ ], React.div' [ ] ]
      count <- liftEffect $ enumerate node >>= length
      count `shouldEqual` 4
    it "iterates over each node"
      do
      ref <- liftEffect $ new ""
      let print node ix =
            do
            tagName <- name node
            let serializer nodeNames = nodeNames <> tagName <> show ix
            modify_ serializer ref
      node <- wrapper_ $ React.div' [ React.span' [ ], React.button' [ ] ]
      liftEffect $ children node >>= forEach print
      html <- liftEffect $ read ref
      html `shouldEqual` "span0button1"
    it "skips nodes"
      do
      node <-
        wrapper_
          $ React.div' [ React.span' [ ], React.button' [ ], React.span' [ ] ]
      html <- liftEffect $ children node >>= skip 1 >>= debug_
      html `shouldEqual` "<button /> <span />"
    it "slices nodes"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div' [ ]
            , React.span' [ ]
            , React.button' [ ]
            , React.span' [ ]
            ]
      html <- liftEffect $ children node >>= slice 1 (-1) >>= debug_
      html `shouldEqual` "<span /> <button />"
    it "reduces the list of nodes"
      do
      let reductor html node index =
            do
            tagName <- name node
            pure $ html <> tagName <> show index
      node <-
        wrapper_
          $ React.div' [ React.span' [ ], React.button' [ ], React.span' [ ] ]
      html <- liftEffect $ children node >>= reduce reductor ""
      html `shouldEqual` "span0button1span2"
  describe "Filtering"
    do
    it "matches when a selector matches"
      do
      node <- wrapper_ $ React.div [ React.className "test" ] [ ]
      match <- liftEffect $ is "*[className='test']" node
      match `shouldEqual` true
    it "does not match when a selector does not match"
      do
      node <- wrapper_ $ React.div' [ ]
      match <- liftEffect $ is "*[className='test']" node
      match `shouldEqual` false
    it "is empty when no selector matches"
      do
      node <- wrapper_ $ React.div' [ ]
      empty <- liftEffect $ find "*[className='test']" node >>= isEmpty
      empty `shouldEqual` true
    it "filters host nodes only"
      do
      node <- wrapper_ $ React.div' [ demo, React.button' [ ], React.span' [ ] ]
      html <- liftEffect $ children node >>= hostNodes >>= debug_
      html `shouldEqual` "<button /> <span />"
    it "matches when a React element pattern matches"
      do
      node <- wrapper_ $ React.span [ React.className "test" ] [ ]
      let pattern = React.span [ React.className "test" ] [ ]
      match <- liftEffect $ matches pattern node
      match `shouldEqual` true
    it "does not match when a React element pattern does not match"
      do
      node <- wrapper_ $ React.span [ React.className "test" ] [ ]
      let pattern = React.span [ React.className "fail" ] [ ]
      match <- liftEffect $ matches pattern node
      match `shouldEqual` false
    it "finds every node that matches a selector"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div [ React.className "test" ] [ ]
            , React.span [ React.className "test" ] [ ]
            ]
      html <- liftEffect $ find "*[className='test']" node >>= debug_
      shouldEqual html "<div className=\"test\" /> <span className=\"test\" />"
    it "finds every node that matches a predicate"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div [ React.className "test" ] [ ]
            , React.span [ React.className "test" ] [ ]
            ]
      html <- liftEffect $ findWhere (is "*[className='test']") node >>= debug_
      shouldEqual html "<div className=\"test\" /> <span className=\"test\" />"
    it "matches when every selector matches"
      do
      node <-
        wrapper_
          $ React.div
            [ React.className "test" ]
            [ React.div [ React.className "test" ] [ ]
            , React.div [ React.className "test" ] [ ]
            ]
      match <- liftEffect $ every "*[className='test']" node
      match `shouldEqual` true
    it "does not match when not every selector matches"
      do
      node <-
        wrapper_
          $ React.div
            [ React.className "test" ]
            [ React.div' [ ]
            , React.div [ React.className "test" ] [ ]
            ]
      match <- liftEffect $ every "*[className='test']" node
      match `shouldEqual` false
    it "matches when every predicate matches"
      do
      node <-
        wrapper_
          $ React.div
            [ React.className "test" ]
            [ React.div [ React.className "test" ] [ ]
            , React.div [ React.className "test" ] [ ]
            ]
      match <- liftEffect $ everyWhere (is "*[className='test']") node
      match `shouldEqual` true
    it "does not match when not every selector matches"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div' [ ], React.div [ React.className "test" ] [ ] ]
      match <- liftEffect $ everyWhere (is "*[className='test']") node
      match `shouldEqual` false
    it "matches when some selector matches"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div [ React.className "test" ] [ ], React.div' [ ] ]
      match <- liftEffect $ some "*[className='test']" node
      match `shouldEqual` true
    it "does not match when no selector matches"
      do
      node <- wrapper_ $ React.div' [ React.div' [ ], React.div' [ ] ]
      match <- liftEffect $ some "*[className='test']" node
      match `shouldEqual` false
    it "matches when some predicate matches"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div [ React.className "test" ] [ ], React.div' [ ] ]
      match <- liftEffect $ someWhere (is "*[className='test']") node
      match `shouldEqual` true
    it "does not match when no predicate matches"
      do
      node <- wrapper_ $ React.div' [ React.div' [ ], React.div' [ ] ]
      match <- liftEffect $ someWhere (is "*[className='test']") node
      match `shouldEqual` false
  describe "Property manipulation"
    do
    it "reads a property's value"
      do
      node <- wrapper_ $ React.div [ React.className "test" ] [ ]
      value <- liftEffect $ stringProperty "className" node
      value `shouldEqual` "test"
    it "writes a property's value"
      do
      node <- wrapper_ $ React.div [ React.className "fail" ] [ ]
      setProperty "className" (unsafeToForeign "test") node
      value <- liftEffect $ stringProperty "className" node
      value `shouldEqual` "test"
    it "invokes a property as a function"
      do
      ref <- liftEffect $ new 0.0
      let clickHandler event =
            do
            x <- clientX event
            modify_ (_ + x) ref
      node <- wrapper_ $ React.button [ React.onClick clickHandler ] [ ]
      liftEffect $ invoke "onClick" node [ unsafeToForeign { clientX : 5.0 } ]
      x <- liftEffect $ read ref
      x `shouldEqual` 5.0
  describe "Printing"
    do
    it "prints the inner text"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div' [ React.span' [ React.text "Hello " ] ]
            , React.span' [ React.text "World!" ]
            ]
      html <- liftEffect $ text node
      html `shouldEqual` "Hello World!"
    it "prints the element name"
      do
      node <- wrapper_ $ React.button' [ ]
      tagName <- liftEffect $ name node
      tagName `shouldEqual` "button"
    it "prints the properties"
      do
      node <- wrapper_ $ React.div [ React.style { color : "red" } ] [ ]
      html <- liftEffect $ debug { ignoreProps : false, verbose : false } node
      html `shouldEqual` "<div style={{...}} />"
    it "prints verbosely"
      do
      node <- wrapper_ $ React.div [ React.style { color : "red" } ] [ ]
      html <- liftEffect $ debug { ignoreProps : false, verbose : true } node
      html `shouldEqual` "<div style={{ color: 'red' }} />"
    it "prints the tree structure"
      do
      node <-
        wrapper_
          $ React.div'
            [ React.div' [ React.text "Hello" ]
            , React.span' [ React.text "World!" ]
            ]
      html <- liftEffect $ debug_ node
      let expectedHtml =
            "<div> <div> Hello </div> <span> World! </span> </div>"
      html `shouldEqual` expectedHtml
  describe "Unmounting"
    do
    it "unmounts the React component"
      do
      ref <- liftEffect $ new 0
      let class_ = React.component "Demo" $ unmountSpec ref
      node <- wrapper_ $ React.createLeafElement class_ { }
      liftEffect $ unmount node
      count <- liftEffect $ read ref
      count `shouldEqual` 1
  where
  wrapper_ = liftEffect <<< wrapper

  debug_ node =
    do
    html <- debug { ignoreProps : false, verbose : false } node
    expression <- either throw pure $ regex "\\s+" global
    pure $ trim $ replace expression " " html

  demo = React.createLeafElement class_ { }
    where
    class_ = React.component "Demo" spec_

    spec_
      :: ReactThis { } { }
      -> Effect { state :: { }, render :: Effect ReactElement }
    spec_ this = pure { state : { }, render : pure $ React.div' [ ] }

  unmountSpec
    :: Ref Int
    -> ReactThis { } { }
    -> Effect
        { componentDidMount :: Effect Unit
        , state :: { }
        , render :: Effect ReactElement
        }
  unmountSpec ref this =
    pure
      { componentDidMount : modify_ (_ + 1) ref
      , state : { }
      , render : pure $ React.div' [ ]
      }

  stringProperty name node =
    do
    value <- property name node
    either (throw <<< show) pure $ runExcept $ readString value
