module Render.Debug exposing (render)

import El exposing (..)
import Html exposing (Html)


render : AnnotatedEl -> Html msg
render ael =
    Html.pre [] [ Html.text <| El.printout ael ]
