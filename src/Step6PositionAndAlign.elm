module Step6PositionAndAlign exposing (positionAndAlign)

import El exposing (..)


positionAndAlign : AnnotatedEl -> AnnotatedEl
positionAndAlign root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            -- each invocation will only change the children, not self
            let
                ( parentX, parentY ) =
                    case maybeParent of
                        Nothing ->
                            ( 0, 0 )

                        Just (AEl parent) ->
                            ( parent.x, parent.y )
            in
            AEl
                { ael
                    | children =
                        -- Not using El.axes as our acc x,y are positional
                        case ael.layoutDirection of
                            LeftToRight ->
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        ( AEl { child | x = accX, y = accY } :: accChildren
                                        , accX + child.width
                                        , accY
                                        )
                                    )
                                    ( [], parentX, parentY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)

                            TopToBottom ->
                                List.foldl
                                    (\(AEl child) ( accChildren, accX, accY ) ->
                                        ( AEl { child | x = accX, y = accY } :: accChildren
                                        , accX
                                        , accY + child.height
                                        )
                                    )
                                    ( [], parentX, parentY )
                                    ael.children
                                    |> (\( newChildren, _, _ ) -> List.reverse newChildren)
                }
        )
        root
