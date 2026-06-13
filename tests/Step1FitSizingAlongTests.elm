module Step1FitSizingAlongTests exposing (suite)

import El exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Test exposing (Test)


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingAlong


suite : Test
suite =
    Test.describe "Step1.fitSizingAlong"
        [ Test.test "default container -> still 0" <|
            \() ->
                Container [] []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
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
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container -> still 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
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
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "grow container -> still 0" <|
            \() ->
                Container
                    [ Height (Grow [])
                    , Width (Grow [])
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
                            , bgColor = Nothing
                            , children = []
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = SGrow
                            , heightMin = Nothing
                            , heightMax = Nothing
                            , widthSpec = SGrow
                            , widthMin = Nothing
                            , widthMax = Nothing
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fixed container LR -> both fixed dimensions filled" <|
            \() ->
                Container
                    [ Height (Fixed 123)
                    , Width (Fixed 456)
                    , LayoutDirection LeftToRight
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 456
                            , height = 123
                            , bgColor = Nothing
                            , children = []
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = SFixed 123
                            , heightMin = Nothing
                            , heightMax = Nothing
                            , widthSpec = SFixed 456
                            , widthMin = Nothing
                            , widthMax = Nothing
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container LR with fixed child -> both same width, container unfilled height" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , LayoutDirection LeftToRight
                    ]
                    [ Container
                        [ Height (Fixed 123)
                        , Width (Fixed 456)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 456
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 456
                                    , height = 123
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 123
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 456
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with fit child -> both 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    [ Container
                        [ Height (Fit [])
                        , Width (Fit [])
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 0
                                    , height = 0
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
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with padding -> use the padding" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , Padding 2 3 5 7
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 3 + 7
                            , height = 0
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
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , paddingTop = 2
                            , paddingRight = 3
                            , paddingBottom = 5
                            , paddingLeft = 7
                            }
                        )
        , Test.test "fit container with child, both with paddings -> use the padding" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , Padding 2 3 5 7
                    ]
                    [ Container [ Padding 20 30 50 70 ] [] ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 3 + 7 + 30 + 70
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 30 + 70
                                    , height = 0
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
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 20
                                    , paddingRight = 30
                                    , paddingBottom = 50
                                    , paddingLeft = 70
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
                            , paddingTop = 2
                            , paddingRight = 3
                            , paddingBottom = 5
                            , paddingLeft = 7
                            }
                        )
        , Test.test "fit container with no children and gap -> don't use the gap" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 0
                            , bgColor = Nothing
                            , children = []
                            , childGap = 1
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with one children and gap -> don't use the gap" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 200
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                ]
                            , childGap = 1
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with two children and gap -> use the gap 1x" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 401
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                ]
                            , childGap = 1
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with two children and gap -> use the gap 1x, vertical" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    , LayoutDirection TopToBottom
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 0
                            , height = 201
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                ]
                            , childGap = 1
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        , Test.test "fit container with three children and gap -> use the gap 2x" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { x = 0
                            , y = 0
                            , width = 602
                            , height = 0
                            , bgColor = Nothing
                            , children =
                                [ AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                , AEl
                                    { x = 0
                                    , y = 0
                                    , width = 200
                                    , height = 100
                                    , bgColor = Nothing
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = SFixed 100
                                    , heightMin = Nothing
                                    , heightMax = Nothing
                                    , widthSpec = SFixed 200
                                    , widthMin = Nothing
                                    , widthMax = Nothing
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , paddingTop = 0
                                    , paddingRight = 0
                                    , paddingBottom = 0
                                    , paddingLeft = 0
                                    }
                                ]
                            , childGap = 1
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
                            , paddingTop = 0
                            , paddingRight = 0
                            , paddingBottom = 0
                            , paddingLeft = 0
                            }
                        )
        ]
