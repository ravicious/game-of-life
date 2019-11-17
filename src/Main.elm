module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.document
        { init = \() -> ( (), Cmd.none )
        , view = \model -> { title = "Game of Life", body = [ view model ] }
        , update = \model msg -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view : a -> Html msg
view _ =
    text "Hello, world!"
