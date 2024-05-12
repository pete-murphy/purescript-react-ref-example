module App where

import Prelude

import Data.Foldable as Foldable
import Data.Nullable as Nullable
import Effect.Class.Console as Console
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget as EventTarget

mkApp :: Component Unit
mkApp = do
  Hooks.component "App" \_ -> Hooks.do
    ref <- Hooks.useRef Nullable.null

    Hooks.useEffect unit do
      maybeNode <- Hooks.readRefMaybe ref
      let
        maybeElement = maybeNode >>= Element.fromNode

      listener <- EventTarget.eventListener \_ -> do
        Console.log "click"

      Foldable.for_ maybeElement \element -> do
        EventTarget.addEventListener (EventType "click") listener true (Element.toEventTarget element)

      pure
        ( Foldable.for_ maybeElement \element -> do
            EventTarget.removeEventListener (EventType "click") listener true (Element.toEventTarget element)
        )

    pure (DOM.div { ref, children: [ DOM.text "Click me!" ] })

