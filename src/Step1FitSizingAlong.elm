module Step1FitSizingAlong exposing (fitSizingAlong)

import El exposing (..)


fitSizingAlong : AnnotatedEl -> AnnotatedEl
fitSizingAlong root =
    El.mapPostOrder
        (\((AEl ael) as ael_) ->
            let
                { along, across } =
                    El.axes ael_

                aelAlong =
                    case along.getSizeSpec ael_ of
                        SFixed n ->
                            ael_ |> along.setSize n

                        SFit ->
                            ael_
                                |> along.setSize
                                    (List.sum (List.map along.getSize ael.children)
                                        + along.getPaddingStart ael_
                                        + along.getPaddingEnd ael_
                                        + {- TODO PERF count them once in Step 0 -} (max 0 (List.length ael.children - 1) * ael.childGap)
                                    )

                        SGrow ->
                            -- Do nothing in this step (see Step2)
                            ael_

                -- TODO: it's unclear to me whether this across step needs to be there in Step1.
                -- See the test "fit container with two children and gap -> use the gap 1x, vertical"
                aelAcross =
                    case across.getSizeSpec ael_ of
                        SFixed n ->
                            aelAlong |> across.setSize n

                        SFit ->
                            -- Do nothing in this step
                            aelAlong

                        SGrow ->
                            -- Do nothing in this step
                            aelAlong
            in
            aelAcross
        )
        root
