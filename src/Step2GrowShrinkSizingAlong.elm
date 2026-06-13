module Step2GrowShrinkSizingAlong exposing (growShrinkSizingAlong)

import El exposing (..)


growShrinkSizingAlong : Config -> AnnotatedEl -> AnnotatedEl
growShrinkSizingAlong c root =
    -- TODO: go from root up (preOrder)
    -- TODO: if it's a Grow, ... TODO
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            let
                { along, across } =
                    El.axes ael_
            in
            case along.sizeSpec of
                SFixed n ->
                    ael_

                SFit ->
                    ael_

                SGrow ->
                    case maybeParent of
                        Nothing ->
                            -- root
                            ael_
                                |> along.setSize (along.getLayoutSize c)

                        Just parent ->
                            ael_
                                |> along.setSize (along.getSize parent)
        )
        root
