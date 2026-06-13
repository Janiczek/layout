module Step4FitSizingAcross exposing (fitSizingAcross)

import El exposing (..)


fitSizingAcross : AnnotatedEl -> AnnotatedEl
fitSizingAcross root =
    El.mapPostOrder
        (\((AEl ael) as ael_) ->
            let
                { across } =
                    El.axes ael_
            in
            case across.getSizeSpec ael_ of
                SFixed n ->
                    ael_ |> across.setSize n

                SFit ->
                    ael_
                        |> across.setSize
                            ((ael.children
                                |> List.map across.getSize
                                |> List.maximum
                                |> Maybe.withDefault 0
                             )
                                + across.getPaddingStart ael_
                                + across.getPaddingEnd ael_
                            )

                SGrow ->
                    -- Do nothing in this step (see Step5)
                    ael_
        )
        root
