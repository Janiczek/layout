module Step1FitSizingWidths exposing (fitSizingWidths)

{-| PERF: Can we do this step during construction (in Step0) instead of a separate traversal?
-}

import El exposing (..)
import Log


fitSizingWidths : AnnotatedEl -> AnnotatedEl
fitSizingWidths root =
    El.mapPostOrder
        (\((AEl ael) as ael_) ->
            if ael.text /= Nothing then
                ael_

            else
                case ael.widthSpec of
                    SFixed n ->
                        AEl { ael | width = max 0 n }

                    SGrow ->
                        -- Don't deal with Grow here - wait for step 2.
                        ael_

                    SFit ->
                        AEl
                            { ael
                                | width =
                                    let
                                        childWidths =
                                            ael.children
                                                |> List.map (El.inner .width)
                                    in
                                    (case ael.layoutDirection of
                                        LeftToRight ->
                                            List.sum childWidths
                                                + -- TODO PERF count them once in Step 0
                                                  max 0 ((List.length ael.children - 1) * ael.childGap)

                                        TopToBottom ->
                                            List.maximum childWidths
                                                |> Maybe.withDefault 0
                                    )
                                        + ael.paddingLeft
                                        + ael.paddingRight
                            }
        )
        root
        |> Log.annotatedEl "step 1 - fit widths"
