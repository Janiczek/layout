module Step5GrowShrinkSizingAcrossTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Log
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Step2GrowShrinkSizingAlong as Step2
import Step3WrapText as Step3 exposing (charHeight, charWidth)
import Step4FitSizingAcross as Step4
import Step5GrowShrinkSizingAcross as Step5
import Test exposing (Test)
import TestHelpers exposing (default)


config : Config
config =
    { layoutWidth = 640
    , layoutHeight = 480
    }


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingAlong
        |> Step2.growShrinkSizingAlong config
        |> Step3.wrapText
        |> Step4.fitSizingAcross
        |> Step5.growShrinkSizingAcross config


suite : Test
suite =
    Test.describe "Step5.growShrinkSizingAcross"
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
        ]
