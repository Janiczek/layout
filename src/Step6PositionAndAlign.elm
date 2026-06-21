module Step6PositionAndAlign exposing (positionAndAlign)

import El exposing (..)
import Log


positionAndAlign : AnnotatedEl -> AnnotatedEl
positionAndAlign root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            -- each invocation will only change the children, not self
            let
                ( startX, startY ) =
                    ( ael.x + ael.paddingLeft
                    , ael.y + ael.paddingTop
                    )
            in
            AEl
                { ael
                    | children =
                        case ael.layoutDirection of
                            LeftToRight ->
                                let
                                    extraWidth =
                                        ael.width
                                            - ael.paddingLeft
                                            - ael.paddingRight
                                            - List.sum (List.map (El.inner .width) ael.children)

                                    offsetX =
                                        case ael.horizAlign of
                                            Left ->
                                                0

                                            HCenter ->
                                                extraWidth // 2

                                            Right ->
                                                extraWidth
                                in
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        let
                                            whitespace =
                                                ael.height
                                                    - ael.paddingTop
                                                    - ael.paddingBottom
                                                    - child.height

                                            offsetY =
                                                case ael.vertAlign of
                                                    Top ->
                                                        0

                                                    VCenter ->
                                                        whitespace // 2

                                                    Bottom ->
                                                        whitespace
                                        in
                                        ( AEl
                                            { child
                                                | x = accX
                                                , y = accY + offsetY
                                            }
                                            :: accChildren
                                        , accX + child.width + ael.childGap
                                        , accY
                                        )
                                    )
                                    ( [], startX + offsetX, startY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)

                            TopToBottom ->
                                let
                                    extraHeight =
                                        ael.height
                                            - ael.paddingTop
                                            - ael.paddingBottom
                                            - List.sum (List.map (El.inner .height) ael.children)

                                    offsetY =
                                        case ael.vertAlign of
                                            Top ->
                                                0

                                            VCenter ->
                                                extraHeight // 2

                                            Bottom ->
                                                extraHeight
                                in
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        let
                                            whitespace =
                                                ael.width
                                                    - ael.paddingTop
                                                    - ael.paddingBottom
                                                    - child.height

                                            offsetX =
                                                case ael.horizAlign of
                                                    Left ->
                                                        0

                                                    HCenter ->
                                                        whitespace // 2

                                                    Right ->
                                                        whitespace
                                        in
                                        ( AEl
                                            { child
                                                | x = accX + offsetX
                                                , y = accY
                                            }
                                            :: accChildren
                                        , accX
                                        , accY + child.height + ael.childGap
                                        )
                                    )
                                    ( [], startX, startY + offsetY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)
                }
        )
        root
