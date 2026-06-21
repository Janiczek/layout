module Step5GrowShrinkSizingAcross exposing (growShrinkSizingAcross)

import El exposing (..)
import Log


growShrinkSizingAcross : Config -> AnnotatedEl -> AnnotatedEl
growShrinkSizingAcross c root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            let
                { across } =
                    El.axes ael_
            in
            case across.getSizeSpec ael_ of
                SFixed n ->
                    ael_

                SFit ->
                    ael_

                SGrow ->
                    case maybeParent of
                        Nothing ->
                            -- root
                            ael_
                                |> across.setSize (across.getLayoutSize c)

                        Just parent ->
                            ael_
                                |> across.setSize (availableSize across parent)
        )
        root


availableSize : Axis -> AnnotatedEl -> Int
availableSize across ((AEl parent_) as parent) =
    -- TODO: Is this the same as taking the maximum of other children?
    {-
       - (parent_.children
           |> List.map across.getSize
           |> List.maximum
           |> Maybe.withDefault 0
       )
    -}
    -- This will likely change when we do some shrinking logic
    across.getSize parent
        - across.getPaddingStart parent
        - across.getPaddingEnd parent
