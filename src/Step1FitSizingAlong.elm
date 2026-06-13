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
                    case along.sizeSpec of
                        SFixed n ->
                            ael_ |> along.setSize n

                        SFit ->
                            ael_
                                |> along.setSize
                                    (List.sum (List.map along.getSize ael.children)
                                        + along.paddingStart
                                        + along.paddingEnd
                                        + {- TODO PERF count them once in Step 0 -} (max 0 (List.length ael.children - 1) * ael.childGap)
                                    )

                        SGrow ->
                            -- Do nothing in this step (see Step2)
                            ael_

                aelAcross =
                    case across.sizeSpec of
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
         {-
            |> (\a ->
                    let
                        _ =
                            Debug.log (El.printout a) ""
                    in
                    a
               )
         -}
        )
        root
