module Layout exposing (layout)

import El exposing (AnnotatedEl, El)
import Step0Annotate as Step0
import Step1FitSizingWidths as Step1
import Step2GrowShrinkSizingWidths as Step2
import Step3WrapText as Step3
import Step4FitSizingHeights as Step4
import Step5GrowShrinkSizingHeights as Step5
import Step6PositionAndAlign as Step6


layout : El -> AnnotatedEl
layout el =
    el
        |> Step0.annotate
        |> Step1.fitSizingWidths
        |> Step2.growShrinkSizingWidths
        |> Step3.wrapText
        |> Step4.fitSizingHeights
        |> Step5.growShrinkSizingHeights
        |> Step6.positionAndAlign
