module Step6PositionAndAlignTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Log
import Step0Annotate as Step0
import Step1FitSizingWidths as Step1
import Step2GrowShrinkSizingWidths as Step2
import Step3WrapText as Step3
import Step4FitSizingHeights as Step4
import Step5GrowShrinkSizingHeights as Step5
import Step6PositionAndAlign as Step6
import Test exposing (Test)
import TestHelpers exposing (default)
import Text


config : Config
config =
    { layoutWidth = 640
    , layoutHeight = 480
    }


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingWidths
        |> Step2.growShrinkSizingWidths config
        |> Step3.wrapText
        |> Step4.fitSizingHeights
        |> Step5.growShrinkSizingHeights config
        |> Step6.positionAndAlign


suite : Test
suite =
    Test.describe "Step6.positionAndAlign"
        [ Test.test "Three items without padding/gap - LR" <|
            \() ->
                Container [ LayoutDirection LeftToRight ]
                    [ Container [ Width (Fixed 50), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 60), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 70), Height (Fixed 70) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 180
                                , height = 70
                                , children =
                                    [ AEl { default | x = 0, y = 0, width = 50, widthSpec = SFixed 50, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 50, y = 0, width = 60, widthSpec = SFixed 60, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 110, y = 0, width = 70, widthSpec = SFixed 70, height = 70, heightSpec = SFixed 70 }
                                    ]
                            }
                        )
        , Test.test "Three items without padding/gap - TB" <|
            \() ->
                Container [ LayoutDirection TopToBottom ]
                    [ Container [ Width (Fixed 50), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 50), Height (Fixed 80) ] []
                    , Container [ Width (Fixed 50), Height (Fixed 90) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 50
                                , height = 240
                                , children =
                                    [ AEl { default | x = 0, y = 0, width = 50, widthSpec = SFixed 50, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 0, y = 70, width = 50, widthSpec = SFixed 50, height = 80, heightSpec = SFixed 80 }
                                    , AEl { default | x = 0, y = 150, width = 50, widthSpec = SFixed 50, height = 90, heightSpec = SFixed 90 }
                                    ]
                            }
                        )
        , Test.fuzz TestHelpers.el "Position of root always at 0,0" <|
            \root ->
                let
                    (AEl root_) =
                        run root
                in
                root_
                    |> Expect.all
                        [ .x >> Expect.equal 0
                        , .y >> Expect.equal 0
                        ]
        , Test.test "Child gap doesn't play role when 0 children - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , ChildGap 3
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 0
                                , height = 0
                                , childGap = 3
                                , children = []
                            }
                        )
        , Test.test "Child gap doesn't play role when 0 children - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , ChildGap 3
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 0
                                , height = 0
                                , childGap = 3
                                , children = []
                            }
                        )
        , Test.test "Child gap doesn't play role when 1 child - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1" ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 7 * Text.charWidth
                                , height = 1 * Text.charHeight
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    ]
                            }
                        )
        , Test.test "Child gap doesn't play role when 1 child - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1" ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 7 * Text.charWidth
                                , height = 1 * Text.charHeight
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    ]
                            }
                        )
        , Test.test "Child gap plays role when 2 children - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1"
                    , Text [] "Child 2"
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 14 * Text.charWidth + 3
                                , height = 1 * Text.charHeight
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 7 * Text.charWidth + 3
                                            , y = 0
                                            , text = Just "Child 2"
                                        }
                                    ]
                            }
                        )
        , Test.test "Child gap plays role when 2 children - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1"
                    , Text [] "Child 2"
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 7 * Text.charWidth
                                , height = 2 * Text.charHeight + 3
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 1 * Text.charHeight + 3
                                            , text = Just "Child 2"
                                        }
                                    ]
                            }
                        )
        , Test.test "Child gap plays role when 3 children - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1"
                    , Text [] "Child 2"
                    , Text [] "Child 3"
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 21 * Text.charWidth + 6
                                , height = 1 * Text.charHeight
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 7 * Text.charWidth + 3
                                            , y = 0
                                            , text = Just "Child 2"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 14 * Text.charWidth + 6
                                            , y = 0
                                            , text = Just "Child 3"
                                        }
                                    ]
                            }
                        )
        , Test.test "Child gap plays role when 3 children - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , ChildGap 3
                    ]
                    [ Text [] "Child 1"
                    , Text [] "Child 2"
                    , Text [] "Child 3"
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 7 * Text.charWidth
                                , height = 3 * Text.charHeight + 6
                                , childGap = 3
                                , children =
                                    [ AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 0
                                            , text = Just "Child 1"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 1 * Text.charHeight + 3
                                            , text = Just "Child 2"
                                        }
                                    , AEl
                                        { default
                                            | width = 7 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , x = 0
                                            , y = 2 * Text.charHeight + 6
                                            , text = Just "Child 3"
                                        }
                                    ]
                            }
                        )
        , Test.test "Padding plays role when 0 children - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , Padding 1 2 3 4
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 6
                                , height = 4
                                , paddingTop = 1
                                , paddingRight = 2
                                , paddingBottom = 3
                                , paddingLeft = 4
                                , children = []
                            }
                        )
        , Test.test "Padding plays role when 0 children - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , Padding 1 2 3 4
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 6
                                , height = 4
                                , paddingTop = 1
                                , paddingRight = 2
                                , paddingBottom = 3
                                , paddingLeft = 4
                                , children = []
                            }
                        )
        , Test.test "Padding plays role when 1 child - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , Padding 1 2 3 4
                    ]
                    [ Text [] "Abc" ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 6 + 3 * Text.charWidth
                                , height = 4 + 1 * Text.charHeight
                                , paddingTop = 1
                                , paddingRight = 2
                                , paddingBottom = 3
                                , paddingLeft = 4
                                , children =
                                    [ AEl
                                        { default
                                            | x = 4
                                            , y = 1
                                            , width = 3 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                            , text = Just "Abc"
                                        }
                                    ]
                            }
                        )
        , Test.test "Padding and child gap - LR" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , ChildGap 3
                    , Padding 1 2 3 4
                    ]
                    [ Container [ Width (Fixed 50), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 60), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 70), Height (Fixed 70) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = LeftToRight
                                , x = 0
                                , y = 0
                                , width = 50 + 60 + 70 + 3 + 3 + 2 + 4
                                , height = 70 + 1 + 3
                                , paddingTop = 1
                                , paddingRight = 2
                                , paddingBottom = 3
                                , paddingLeft = 4
                                , childGap = 3
                                , children =
                                    [ AEl { default | x = 4, y = 1, width = 50, widthSpec = SFixed 50, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 57, y = 1, width = 60, widthSpec = SFixed 60, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 120, y = 1, width = 70, widthSpec = SFixed 70, height = 70, heightSpec = SFixed 70 }
                                    ]
                            }
                        )
        , Test.test "Padding and child gap - TB" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , ChildGap 3
                    , Padding 1 2 3 4
                    ]
                    [ Container [ Width (Fixed 50), Height (Fixed 70) ] []
                    , Container [ Width (Fixed 50), Height (Fixed 80) ] []
                    , Container [ Width (Fixed 50), Height (Fixed 90) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , x = 0
                                , y = 0
                                , width = 50 + 2 + 4
                                , height = 70 + 80 + 90 + 3 + 3 + 1 + 3
                                , paddingTop = 1
                                , paddingRight = 2
                                , paddingBottom = 3
                                , paddingLeft = 4
                                , childGap = 3
                                , children =
                                    [ AEl { default | x = 4, y = 1, width = 50, widthSpec = SFixed 50, height = 70, heightSpec = SFixed 70 }
                                    , AEl { default | x = 4, y = 74, width = 50, widthSpec = SFixed 50, height = 80, heightSpec = SFixed 80 }
                                    , AEl { default | x = 4, y = 157, width = 50, widthSpec = SFixed 50, height = 90, heightSpec = SFixed 90 }
                                    ]
                            }
                        )
        , Test.test "VertAlign Top" <|
            \() ->
                Container
                    [ VertAlign Top
                    , HorizAlign Left
                    , Width (Fixed 100)
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fixed 10)
                        , Height (Fixed 10)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | vertAlign = Top
                                , horizAlign = Left
                                , widthSpec = SFixed 100
                                , heightSpec = SFixed 100
                                , width = 100
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , height = 10
                                            , widthSpec = SFixed 10
                                            , heightSpec = SFixed 10
                                            , x = 0
                                            , y = 0
                                        }
                                    ]
                            }
                        )
        , Test.test "VertAlign VCenter" <|
            \() ->
                Container
                    [ VertAlign VCenter
                    , HorizAlign Left
                    , Width (Fixed 100)
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fixed 10)
                        , Height (Fixed 10)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | vertAlign = VCenter
                                , horizAlign = Left
                                , widthSpec = SFixed 100
                                , heightSpec = SFixed 100
                                , width = 100
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , height = 10
                                            , widthSpec = SFixed 10
                                            , heightSpec = SFixed 10
                                            , x = 0
                                            , y = 45 -- 100/2 - 10/2
                                        }
                                    ]
                            }
                        )
        , Test.only <|
            Test.test "VertAlign Bottom" <|
                \() ->
                    Container
                        [ VertAlign Bottom
                        , HorizAlign Left
                        , Width (Fixed 100)
                        , Height (Fixed 100)
                        ]
                        [ Container
                            [ Width (Fixed 10)
                            , Height (Fixed 10)
                            ]
                            []
                        ]
                        |> run
                        |> TestHelpers.expectEqualAnnotatedEl
                            (AEl
                                { default
                                    | vertAlign = Bottom
                                    , horizAlign = Left
                                    , widthSpec = SFixed 100
                                    , heightSpec = SFixed 100
                                    , width = 100
                                    , height = 100
                                    , children =
                                        [ AEl
                                            { default
                                                | width = 10
                                                , height = 10
                                                , widthSpec = SFixed 10
                                                , heightSpec = SFixed 10
                                                , x = 0
                                                , y = 90 -- 100 - 10
                                            }
                                        ]
                                }
                            )
        , Test.test "HorizAlign Left" <|
            \() ->
                Container
                    [ VertAlign Top
                    , HorizAlign Left
                    , Width (Fixed 100)
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fixed 10)
                        , Height (Fixed 10)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | vertAlign = Top
                                , horizAlign = Left
                                , widthSpec = SFixed 100
                                , heightSpec = SFixed 100
                                , width = 100
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , height = 10
                                            , widthSpec = SFixed 10
                                            , heightSpec = SFixed 10
                                            , x = 0
                                            , y = 0
                                        }
                                    ]
                            }
                        )
        , Test.test "HorizAlign HCenter" <|
            \() ->
                Container
                    [ VertAlign Top
                    , HorizAlign HCenter
                    , Width (Fixed 100)
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fixed 10)
                        , Height (Fixed 10)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | vertAlign = Top
                                , horizAlign = HCenter
                                , widthSpec = SFixed 100
                                , heightSpec = SFixed 100
                                , width = 100
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , height = 10
                                            , widthSpec = SFixed 10
                                            , heightSpec = SFixed 10
                                            , x = 45 -- 100/2 - 10/2
                                            , y = 0
                                        }
                                    ]
                            }
                        )
        , Test.test "HorizAlign Right" <|
            \() ->
                Container
                    [ VertAlign Top
                    , HorizAlign Right
                    , Width (Fixed 100)
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fixed 10)
                        , Height (Fixed 10)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | vertAlign = Top
                                , horizAlign = Right
                                , widthSpec = SFixed 100
                                , heightSpec = SFixed 100
                                , width = 100
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , height = 10
                                            , widthSpec = SFixed 10
                                            , heightSpec = SFixed 10
                                            , x = 90 -- 100 - 10
                                            , y = 0
                                        }
                                    ]
                            }
                        )
        ]
