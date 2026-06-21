module Render.HtmlTranslate exposing (render)

{-| Render the layout by making all elements live at (0,0) via CSS grid and translating them via
transform: translate2D(x,y).
-}

import El exposing (AnnotatedEl(..), Color(..))
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Extra


render : AnnotatedEl -> Html msg
render ((AEl root) as root_) =
    let
        els : List AnnotatedEl
        els =
            El.preOrder root_
    in
    Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-areas" "\"a\""
        ]
        (List.map viewEl els)


viewEl : AnnotatedEl -> Html msg
viewEl (AEl el) =
    Html.div
        [ Html.Attributes.style "grid-area" "a"
        , Html.Attributes.style "transform" ("translate2D(" ++ String.fromInt el.x ++ "," ++ String.fromInt el.y ++ ")")
        , Html.Attributes.style "width" (String.fromInt el.width)
        , Html.Attributes.style "height" (String.fromInt el.height)
        , el.bgColor
            |> Html.Attributes.Extra.attributeMaybe
                (\color ->
                    Html.Attributes.style "background-color" <|
                        case color of
                            Blue ->
                                "blue"

                            Purple ->
                                "purple"

                            LightPurple ->
                                "plum"

                            Pink ->
                                "pink"

                            Yellow ->
                                "yellow"
                )
        ]
        [ el.text
            |> Html.Extra.viewMaybe
                -- TODO pixel font
                (\text -> Html.text text)
        ]
