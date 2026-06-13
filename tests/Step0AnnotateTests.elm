module Step0AnnotateTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Test exposing (Test)
import TestHelpers


suite : Test
suite =
    Test.describe "Step0.annotate"
        [ Test.test "ex1 - purple root with no children" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , BgColor Purple
                    ]
                    []
                    |> Step0.annotate
                    |> Expect.equal
                        (AEl
                            { x = 0, y = 0, width = 0, height = 0
                            , bgColor = Just Purple
                            , children = []
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = TopToBottom
                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                            }
                        )
        , Test.test "ex2 - purple menu with two light-purple items" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , BgColor Purple
                    ]
                    [ Container
                        [ Width (Grow [])
                        , BgColor LightPurple
                        ]
                        [ Text [ FontSize 18 ] "Copy"
                        , Container [] [ Text [] "copy" ]
                        ]
                    , Container
                        [ Width (Grow [])
                        , BgColor LightPurple
                        ]
                        [ Text [ FontSize 18 ] "Paste"
                        , Container [] [ Text [] "paste" ]
                        ]
                    ]
                    |> Step0.annotate
                    |> Expect.equal
                        (AEl
                            { x = 0, y = 0, width = 0, height = 0
                            , bgColor = Just Purple
                            , children =
                                [ AEl
                                    { x = 0, y = 0, width = 0, height = 0
                                    , bgColor = Just LightPurple
                                    , children =
                                        [ AEl
                                            { x = 0, y = 0, width = 0, height = 0
                                            , bgColor = Nothing
                                            , children = []
                                            , childGap = 0
                                            , fontSize = Just 18
                                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Just "Copy"
                                            , layoutDirection = LeftToRight
                                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                            }
                                        , AEl
                                            { x = 0, y = 0, width = 0, height = 0
                                            , bgColor = Nothing
                                            , children =
                                                [ AEl
                                                    { x = 0, y = 0, width = 0, height = 0
                                                    , bgColor = Nothing
                                                    , children = []
                                                    , childGap = 0
                                                    , fontSize = Nothing
                                                    , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                                    , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                                    , horizAlign = HCenter
                                                    , vertAlign = VCenter
                                                    , text = Just "copy"
                                                    , layoutDirection = LeftToRight
                                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                                    }
                                                ]
                                            , childGap = 0
                                            , fontSize = Nothing
                                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Nothing
                                            , layoutDirection = LeftToRight
                                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                            }
                                        ]
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                    , widthSpec = SGrow
                            , widthMin = Nothing
                            , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0, y = 0, width = 0, height = 0
                                    , bgColor = Just LightPurple
                                    , children =
                                        [ AEl
                                            { x = 0, y = 0, width = 0, height = 0
                                            , bgColor = Nothing
                                            , children = []
                                            , childGap = 0
                                            , fontSize = Just 18
                                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Just "Paste"
                                            , layoutDirection = LeftToRight
                                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                            }
                                        , AEl
                                            { x = 0, y = 0, width = 0, height = 0
                                            , bgColor = Nothing
                                            , children =
                                                [ AEl
                                                    { x = 0, y = 0, width = 0, height = 0
                                                    , bgColor = Nothing
                                                    , children = []
                                                    , childGap = 0
                                                    , fontSize = Nothing
                                                    , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                                    , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                                    , horizAlign = HCenter
                                                    , vertAlign = VCenter
                                                    , text = Just "paste"
                                                    , layoutDirection = LeftToRight
                                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                                    }
                                                ]
                                            , childGap = 0
                                            , fontSize = Nothing
                                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Nothing
                                            , layoutDirection = LeftToRight
                                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                            }
                                        ]
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                                    , widthSpec = SGrow
                            , widthMin = Nothing
                            , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                    }
                                ]
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = SFit
                            , heightMin = Nothing
                            , heightMax = Nothing
                            , widthSpec = SFit
                            , widthMin = Nothing
                            , widthMax = Nothing
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = TopToBottom
                            , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                            }
                        )
        , Test.test "ex3 - blue container with pink and yellow children" <|
            \() ->
                Container
                    [ Width (Fixed 960)
                    , Height (Fixed 540)
                    , BgColor Blue
                    , Padding 32 32 32 32
                    , ChildGap 32
                    ]
                    [ Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        , BgColor Pink
                        ]
                        []
                    , Container
                        [ Width (Fixed 350)
                        , Height (Fixed 200)
                        , BgColor Yellow
                        ]
                        []
                    ]
                    |> Step0.annotate
                    |> Expect.equal
                        (AEl
                            { x = 0, y = 0, width = 0, height = 0
                            , bgColor = Just Blue
                            , children =
                                [ AEl
                                    { x = 0, y = 0, width = 0, height = 0
                                    , bgColor = Just Pink
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , widthSpec = SFixed 300
                            , widthMin = Nothing
                            , widthMax = Nothing
                                    , heightSpec = SFixed 300
                            , heightMin = Nothing
                            , heightMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0, y = 0, width = 0, height = 0
                                    , bgColor = Just Yellow
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , widthSpec = SFixed 350
                            , widthMin = Nothing
                            , widthMax = Nothing
                                    , heightSpec = SFixed 200
                            , heightMin = Nothing
                            , heightMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0, paddingRight = 0, paddingBottom = 0, paddingLeft = 0
                                    }
                                ]
                            , childGap = 32
                            , fontSize = Nothing
                            , widthSpec = SFixed 960
                            , widthMin = Nothing
                            , widthMax = Nothing
                            , heightSpec = SFixed 540
                            , heightMin = Nothing
                            , heightMax = Nothing
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 32, paddingRight = 32, paddingBottom = 32, paddingLeft = 32
                            }
                        )
        , Test.fuzz TestHelpers.el "position and size are zero" <|
            \input ->
                input
                    |> Step0.annotate
                    |> El.preOrder
                    |> List.all isZeroed
                    |> Expect.equal True

        -- TODO test that the attr properties are preserved?
        , Test.fuzz (Fuzz.list TestHelpers.el) "tree structure is preserved" <|
            \children ->
                let
                    el =
                        Container [] children
                in
                el
                    |> Step0.annotate
                    |> childrenPreserved el
                    |> Expect.equal True
        ]


isZeroed : AnnotatedEl -> Bool
isZeroed annotated =
    case annotated of
        AEl { x, y, width, height } ->
            (x == 0)
                && (y == 0)
                && (width == 0)
                && (height == 0)


childrenPreserved : El -> AnnotatedEl -> Bool
childrenPreserved el annotated =
    case ( el, annotated ) of
        ( Container _ elChildren, AEl { children } ) ->
            (List.length elChildren == List.length children)
                && (List.map2 childrenPreserved elChildren children
                        |> List.all identity
                   )

        ( Text _ _, AEl { children } ) ->
            List.isEmpty children

        ( Empty, AEl { children } ) ->
            List.isEmpty children
