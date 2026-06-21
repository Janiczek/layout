module Step5GrowShrinkSizingHeightsTests exposing (suite)

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


suite : Test
suite =
    Test.describe "Step5.growShrinkSizingHeights"
        [ Test.test "Example from video" <|
            \() ->
                Container
                    [ Width (Fixed 1200)
                    , Height (Fit [])
                    , ChildGap 32
                    , Padding 32 32 32 32
                    , LayoutDirection LeftToRight
                    ]
                    [ Container [ Width (Fixed 40), Height (Fixed 20) ] []
                    , Container [ Width (Fixed 300), Height (Grow []) ] []
                    , Container [] [ Container [ Width (Fixed 70), Height (Fixed 40) ] [] ]
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 1200
                                , widthSpec = SFixed 1200
                                , height = 104
                                , heightSpec = SFit
                                , childGap = 32
                                , paddingLeft = 32
                                , paddingRight = 32
                                , paddingTop = 32
                                , paddingBottom = 32
                                , layoutDirection = LeftToRight
                                , children =
                                    [ AEl
                                        { default
                                            | width = 40
                                            , widthSpec = SFixed 40
                                            , height = 20
                                            , heightSpec = SFixed 20
                                        }
                                    , AEl
                                        { default
                                            | width = 300
                                            , widthSpec = SFixed 300
                                            , height = 40
                                            , heightSpec = SGrow
                                        }
                                    , AEl
                                        { default
                                            | width = 70
                                            , height = 40
                                            , children =
                                                [ AEl
                                                    { default
                                                        | width = 70
                                                        , widthSpec = SFixed 70
                                                        , height = 40
                                                        , heightSpec = SFixed 40
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        , Test.test "grow example from video - growth in both axes" <|
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
                                , heightSpec = SFit
                                , height = 364
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
                                            , height = 300
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SGrow
                                            , width = 872
                                            , height = 300
                                        }
                                    , AEl
                                        { default
                                            | widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                            , width = 300
                                            , height = 300
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
                                , height = 480
                                , widthSpec = SGrow
                                , heightSpec = SGrow
                                , children =
                                    [ AEl
                                        { default
                                            | width = 640
                                            , height = 1 * Text.charHeight
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl
                                                    { default
                                                        | width = 640 - 5 * Text.charWidth
                                                        , height = 1 * Text.charHeight
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
                                                        , height = 1 * Text.charHeight
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
        ]
