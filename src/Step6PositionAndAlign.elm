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
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        ( AEl
                                            { child
                                                | x = accX
                                                , y = accY
                                            }
                                            :: accChildren
                                        , accX + child.width + ael.childGap
                                        , accY
                                        )
                                    )
                                    ( [], startX, startY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)

                            TopToBottom ->
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        ( AEl
                                            { child
                                                | x = accX
                                                , y = accY
                                            }
                                            :: accChildren
                                        , accX
                                        , accY + child.height + ael.childGap
                                        )
                                    )
                                    ( [], startX, startY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)
                }
        )
        root
