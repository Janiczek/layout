module Layout exposing (Config, layout)

import El exposing (AnnotatedEl, El)
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Step2GrowShrinkSizingAlong as Step2
import Step3WrapText as Step3
import Step4FitSizingAcross as Step4
import Step5GrowShrinkSizingAcross as Step5
import Step6PositionAndAlign as Step6


type alias Config =
    { layoutWidth : Int
    , layoutHeight : Int
    }


layout : Config -> El -> AnnotatedEl
layout c el =
    el
        |> Step0.annotate
        |> Step1.fitSizingAlong
        |> Step2.growShrinkSizingAlong c
        |> Step3.wrapText
        |> Step4.fitSizingAcross
        |> Step5.growShrinkSizingAcross
        |> Step6.positionAndAlign
