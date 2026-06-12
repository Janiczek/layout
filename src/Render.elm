module Render exposing (webgl, htmlTranslate, htmlGrid)

import Html exposing (Html)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import El exposing (AnnotatedEl(..))
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)

webgl : AnnotatedEl -> Html msg
webgl ael =
    Debug.todo "webgl"

htmlTranslate : AnnotatedEl -> Html msg
htmlTranslate ael =
    Debug.todo "htmlTranslate"

htmlGrid : AnnotatedEl -> Html msg
htmlGrid ael =
    Debug.todo "htmlGrid"