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
                                        max 0 <|
                                            ael.width
                                                - ael.paddingLeft
                                                - ael.paddingRight
                                                - List.sum (List.map (El.inner .width) ael.children)
                                                - ((List.length ael.children - 1) * ael.childGap)

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
                                            whitespaceY =
                                                ael.height
                                                    - ael.paddingTop
                                                    - ael.paddingBottom
                                                    - child.height

                                            offsetY =
                                                case ael.vertAlign of
                                                    Top ->
                                                        0

                                                    VCenter ->
                                                        whitespaceY // 2

                                                    Bottom ->
                                                        whitespaceY
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
                                        max 0 <|
                                            ael.height
                                                - ael.paddingTop
                                                - ael.paddingBottom
                                                - List.sum (List.map (El.inner .height) ael.children)
                                                - ((List.length ael.children - 1) * ael.childGap)

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
                                            whitespaceX =
                                                ael.width
                                                    - ael.paddingLeft
                                                    - ael.paddingRight
                                                    - child.width

                                            offsetX =
                                                case ael.horizAlign of
                                                    Left ->
                                                        0

                                                    HCenter ->
                                                        whitespaceX // 2

                                                    Right ->
                                                        whitespaceX
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
