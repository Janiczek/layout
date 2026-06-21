module Step4FitSizingHeights exposing (fitSizingHeights)

import El exposing (..)
import Log


fitSizingHeights : AnnotatedEl -> AnnotatedEl
fitSizingHeights root =
    El.mapPostOrder
        (\((AEl ael) as ael_) ->
            if ael.text /= Nothing then
                -- text is already sized
                ael_

            else
                case ael.heightSpec of
                    SFixed n ->
                        AEl { ael | height = n }

                    SGrow ->
                        -- Don't deal with Grow here - wait for step 5.
                        ael_

                    SFit ->
                        AEl
                            { ael
                                | height =
                                    case ael.layoutDirection of
                                        LeftToRight ->
                                            (ael.children
                                                |> List.map (El.inner .height)
                                                |> List.maximum
                                                |> Maybe.withDefault 0
                                            )
                                                + ael.paddingTop
                                                + ael.paddingBottom

                                        TopToBottom ->
                                            (ael.children
                                                |> List.map (El.inner .height)
                                                |> List.sum
                                            )
                                                + ael.paddingTop
                                                + ael.paddingBottom
                                                + max 0 ((List.length ael.children - 1) * ael.childGap)
                            }
        )
        root
        |> Log.annotatedEl "step 4 - fit heights"
