module Step2GrowShrinkSizingAlong exposing (Config, growShrinkSizingAlong)

import El exposing (AnnotatedEl, El(..))


type alias Config c =
    { c
        | layoutWidth : Int
        , layoutHeight : Int
    }


growShrinkSizingAlong : Config c -> AnnotatedEl -> AnnotatedEl
growShrinkSizingAlong c ael =
    Debug.todo "growShrinkSizingAlong"
