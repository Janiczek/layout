module Render.HtmlAbsolute exposing (render)

{-| Render the layout by making all elements position:absolute inside an position:relative root.
-}

import El exposing (AnnotatedEl(..), Color(..))
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Extra
import Text


render : AnnotatedEl -> Html msg
render ((AEl root) as root_) =
    let
        els : List AnnotatedEl
        els =
            El.preOrder root_
    in
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" (String.fromInt root.width ++ "px")
        , Html.Attributes.style "height" (String.fromInt root.height ++ "px")
        ]
        (List.map viewEl els)


viewEl : AnnotatedEl -> Html msg
viewEl (AEl el) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (String.fromInt el.x ++ "px")
        , Html.Attributes.style "top" (String.fromInt el.y ++ "px")
        , Html.Attributes.style "width" (String.fromInt el.width ++ "px")
        , Html.Attributes.style "height" (String.fromInt el.height ++ "px")
        , el.bgColor
            |> Html.Attributes.Extra.attributeMaybe
                (\color ->
                    Html.Attributes.style "background-color" <| El.colorToHtmlColor color
                )
        ]
        [ el.text
            |> Html.Extra.viewMaybe
                (Text.viewHtml
                    (el.fontColor |> Maybe.withDefault Black)
                    el.width
                    el.height
                )
        ]
