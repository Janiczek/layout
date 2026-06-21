module Step2GrowShrinkSizingWidths exposing (growShrinkSizingWidths)

import El exposing (..)
import Log


growShrinkSizingWidths : Config -> AnnotatedEl -> AnnotatedEl
growShrinkSizingWidths c root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            case ael.widthSpec of
                SFixed n ->
                    -- Don't deal with Fixed here - we have done that in step 1.
                    ael_

                SFit ->
                    -- Don't deal with Fit here - we have done that in step 1.
                    ael_

                SGrow ->
                    AEl
                        { ael
                            | width =
                                case maybeParent of
                                    Nothing ->
                                        -- root
                                        c.layoutWidth

                                    Just parent ->
                                        availableSize parent
                        }
        )
        root
        |> Log.annotatedEl "step 2 - grow/shrink widths"


availableSize : AnnotatedEl -> Int
availableSize ((AEl parent_) as parent) =
    -- TODO shrinking calculations, probably will replace the max 0 here
    max 0 <|
        parent_.width
            - parent_.paddingLeft
            - parent_.paddingRight
            - -- TODO perf children count
              ((List.length parent_.children - 1) * parent_.childGap)
            - List.sum (List.map (El.inner .width) parent_.children)
