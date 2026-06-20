module Step6PositionAndAlignTests exposing (suite)

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
import Step6PositionAndAlign as Step6
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
        ]
