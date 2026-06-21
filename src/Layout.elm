module Layout exposing (layout)

import El exposing (AnnotatedEl, Config, El)
import Step0Annotate as Step0
import Step1FitSizingWidths as Step1
import Step2GrowShrinkSizingWidths as Step2
import Step3WrapText as Step3
import Step4FitSizingHeights as Step4
import Step5GrowShrinkSizingHeights as Step5
import Step6PositionAndAlign as Step6


layout : Config -> El -> AnnotatedEl
layout c el =
    el
        |> Step0.annotate
        |> Step1.fitSizingWidths
        |> Step2.growShrinkSizingWidths c
        |> Step3.wrapText
        |> Step4.fitSizingHeights
        |> Step5.growShrinkSizingHeights c
        |> Step6.positionAndAlign
