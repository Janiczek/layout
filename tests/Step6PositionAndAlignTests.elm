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
        [ Test.todo "Some tests"
        ]
