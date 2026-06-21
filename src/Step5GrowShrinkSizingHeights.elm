module Step5GrowShrinkSizingHeights exposing (growShrinkSizingHeights)

import El exposing (..)
import Log


growShrinkSizingHeights : Config -> AnnotatedEl -> AnnotatedEl
growShrinkSizingHeights c root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            if ael.text /= Nothing then
                -- text is already sized
                ael_

            else
                case ael.heightSpec of
                    SFixed n ->
                        -- Don't deal with Fixed here - we have done that in step 4.
                        ael_

                    SFit ->
                        -- Don't deal with Fit here - we have done that in step 4.
                        ael_

                    SGrow ->
                        AEl
                            { ael
                                | height =
                                    case maybeParent of
                                        Nothing ->
                                            -- root
                                            c.layoutHeight

                                        Just parent ->
                                            availableSize parent
                            }
        )
        root
        |> Log.annotatedEl "step 5 - grow/shrink heights"


availableSize : AnnotatedEl -> Int
availableSize ((AEl parent_) as parent) =
    -- TODO: Is this the same as taking the maximum of other children?
    {-
       - (parent_.children
           |> List.map across.getSize
           |> List.maximum
           |> Maybe.withDefault 0
       )
    -}
    -- This will likely change when we do some shrinking logic
    parent_.height
        - parent_.paddingTop
        - parent_.paddingBottom
