module Main exposing (main)

import App exposing (..)
import Browser


main : Program {} Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
