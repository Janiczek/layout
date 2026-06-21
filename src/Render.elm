module Render exposing
    ( debug
    , htmlAbsolute
    , htmlCss
    , htmlGrid
    , htmlTranslate
    , svg
    , webgl
    )

import Browser
import Browser.Events
import El exposing (AnnotatedEl(..))
import Html exposing (Html)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Debug
import Render.HtmlTranslate
import WebGL exposing (Mesh, Shader)


webgl : AnnotatedEl -> Html msg
webgl ael =
    Debug.todo "webgl"


svg : AnnotatedEl -> Html msg
svg ael =
    Debug.todo "svg"


htmlCss : AnnotatedEl -> Html msg
htmlCss ael =
    Debug.todo "htmlCss"


htmlTranslate : AnnotatedEl -> Html msg
htmlTranslate ael =
    Render.HtmlTranslate.render ael


htmlAbsolute : AnnotatedEl -> Html msg
htmlAbsolute ael =
    Debug.todo "htmlAbsolute"


htmlGrid : AnnotatedEl -> Html msg
htmlGrid ael =
    Debug.todo "htmlGrid"


debug : AnnotatedEl -> Html msg
debug ael =
    Render.Debug.render ael
