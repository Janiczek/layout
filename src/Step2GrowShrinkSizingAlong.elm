module Step2GrowShrinkSizingAlong exposing (growShrinkSizingAlong)

import El exposing (..)
import Log


growShrinkSizingAlong : Config -> AnnotatedEl -> AnnotatedEl
growShrinkSizingAlong c root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            let
                { along } =
                    El.axes ael_
            in
            case along.getSizeSpec ael_ of
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
                                |> along.setSize (availableSize along parent)
        )
        root


availableSize : Axis -> AnnotatedEl -> Int
availableSize along ((AEl parent_) as parent) =
    along.getSize parent
        - along.getPaddingStart parent
        - along.getPaddingEnd parent
        - -- TODO perf children count
          ((List.length parent_.children - 1) * parent_.childGap)
        - List.sum (List.map along.getSize parent_.children)
