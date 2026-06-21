module Step2GrowShrinkSizingWidthsTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Step1FitSizingWidths as Step1
import Step2GrowShrinkSizingWidths as Step2
import Test exposing (Test)
import TestHelpers exposing (default)
import Text


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingWidths
        |> Step2.growShrinkSizingWidths
            { layoutWidth = 640
            , layoutHeight = 480
            }


suite : Test
suite =
    Test.describe "Step2.growShrinkSizingWidths"
        [ Test.test "default container -> width still 0" <|
            \() ->
                Container [] []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 0
                            }
                        )
        , Test.test "fit container -> width still 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 0
                            }
                        )
        , Test.fuzz TestHelpers.layoutDirectionFuzzer "grow root -> takes layout width" <|
            \ld ->
                Container
                    [ Height (Grow [])
                    , Width (Grow [])
                    , LayoutDirection ld
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | heightSpec = SGrow
                                , widthSpec = SGrow
                                , layoutDirection = ld
                                , width = 640
                            }
                        )
        , Test.fuzz TestHelpers.layoutDirectionFuzzer "grow child -> takes parent width" <|
            \ld ->
                Container
                    [ Height (Fixed 300)
                    , Width (Fixed 200)
                    ]
                    [ Container
                        [ Width (Grow [])
                        , LayoutDirection ld
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | heightSpec = SFixed 300
                                , widthSpec = SFixed 200
                                , width = 200
                                , height = 0
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SGrow
                                            , width = 200
                                            , layoutDirection = ld
                                        }
                                    ]
                            }
                        )
        , Test.test "grow example from video" <|
            \() ->
                Container
                    [ Width (Fixed 1600)
                    , Padding 32 32 32 32
                    , ChildGap 32
                    ]
                    [ Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        ]
                        []
                    , Container
                        [ Width (Grow [])
                        , Height (Fixed 200)
                        ]
                        []
                    , Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed 1600
                                , width = 1600
                                , height = 0
                                , paddingLeft = 32
                                , paddingRight = 32
                                , paddingTop = 32
                                , paddingBottom = 32
                                , childGap = 32
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                            , width = 300
                                            , height = 0
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SFixed 200
                                            , width = 872
                                            , height = 0
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                            , width = 300
                                            , height = 0
                                        }
                                    ]
                            }
                        )
        , Test.test "grow example from video - but with grow in other axis too" <|
            \() ->
                Container
                    [ Width (Fixed 1600)
                    , Padding 32 32 32 32
                    , ChildGap 32
                    ]
                    [ Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        ]
                        []
                    , Container
                        [ Width (Grow [])
                        , Height (Grow [])
                        ]
                        []
                    , Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        ]
                        []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed 1600
                                , width = 1600
                                , height = 0
                                , paddingLeft = 32
                                , paddingRight = 32
                                , paddingTop = 32
                                , paddingBottom = 32
                                , childGap = 32
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                            , width = 300
                                            , height = 0
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SGrow
                                            , width = 872
                                            , height = 0
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                            , width = 300
                                            , height = 0
                                        }
                                    ]
                            }
                        )
        , Test.test "grow parent of text should have width filled" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , Width (Grow [])
                    ]
                    [ Container
                        [ Width (Grow []) -- this should have the viewport size
                        ]
                        [ Text [] "Title" ]
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SGrow
                                , width = 640
                                , heightSpec = SFit
                                , height = 0
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SFit
                                            , width = 640
                                            , height = 0
                                            , children =
                                                [ AEl
                                                    { default
                                                        | widthSpec = SFit
                                                        , heightSpec = SFit
                                                        , width = 5 * Text.charWidth
                                                        , height = 1 * Text.charHeight
                                                        , text = Just "Title"
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        , Test.test "regression - part of holy grail - TB root, LR child not growing properly" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , Width (Grow [])
                    , Height (Grow [])
                    ]
                    [ Container
                        [ LayoutDirection LeftToRight
                        , Width (Grow [])
                        ]
                        [ Container
                            [ Width (Grow []) ]
                            [ Text [] "TitleX" ]
                        , Container
                            []
                            [ Text [] "Login" ]
                        ]
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | layoutDirection = TopToBottom
                                , width = 640
                                , height = 0
                                , widthSpec = SGrow
                                , heightSpec = SGrow
                                , children =
                                    [ AEl
                                        { default
                                            | width = 640
                                            , height = 0
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl
                                                    { default
                                                        | width = 640 - 5 * Text.charWidth
                                                        , height = 0
                                                        , widthSpec = SGrow
                                                        , children =
                                                            [ AEl
                                                                { default
                                                                    | width = 6 * Text.charWidth
                                                                    , height = 1 * Text.charHeight
                                                                    , text = Just "TitleX"
                                                                }
                                                            ]
                                                    }
                                                , AEl
                                                    { default
                                                        | width = 5 * Text.charWidth
                                                        , height = 0
                                                        , children =
                                                            [ AEl
                                                                { default
                                                                    | width = 5 * Text.charWidth
                                                                    , height = 1 * Text.charHeight
                                                                    , text = Just "Login"
                                                                }
                                                            ]
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        , Test.test "Regression - negative width" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom ]
                    [ Container
                        [ Width (Grow [])
                        , Padding 0 0 0 0
                        , Padding 0 0 0 0
                        ]
                        []
                    , Container [ Padding 0 0 0 1 ] []
                    , Text [] " "
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 1 * Text.charWidth
                                , height = 0
                                , layoutDirection = TopToBottom
                                , children =
                                    [ AEl { default | widthSpec = SGrow }
                                    , AEl
                                        { default
                                            | paddingLeft = 1
                                            , width = 1
                                        }
                                    , AEl
                                        { default
                                            | text = Just " "
                                            , width = 1 * Text.charWidth
                                            , height = 1 * Text.charHeight
                                        }
                                    ]
                            }
                        )
        ]
