module Step4FitSizingAcrossTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Log
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Step2GrowShrinkSizingAlong as Step2
import Step3WrapText as Step3 exposing (charHeight, charWidth)
import Step4FitSizingAcross as Step4
import Test exposing (Test)
import TestHelpers exposing (default)


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingAlong
        |> Step2.growShrinkSizingAlong
            { layoutWidth = 640
            , layoutHeight = 480
            }
        |> Step3.wrapText
        |> Step4.fitSizingAcross


suite : Test
suite =
    Test.describe "Step4.fitSizingAcross"
        [ Test.test "Fit LR container with fixed children - height (across) filled in" <|
            \() ->
                Container
                    [ Width (Fit [])
                    , Height (Fit [])
                    , ChildGap 2
                    , Padding 5 5 5 5
                    , LayoutDirection LeftToRight
                    ]
                    [ Container [ Width (Fixed 10), Height (Fixed 10) ] []
                    , Container [ Width (Fixed 20), Height (Fixed 20) ] []
                    , Container [ Width (Fixed 30), Height (Fixed 30) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 74
                                , widthSpec = SFit
                                , height = 40
                                , heightSpec = SFit
                                , childGap = 2
                                , paddingLeft = 5
                                , paddingRight = 5
                                , paddingTop = 5
                                , paddingBottom = 5
                                , layoutDirection = LeftToRight
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , widthSpec = SFixed 10
                                            , height = 10
                                            , heightSpec = SFixed 10
                                        }
                                    , AEl
                                        { default
                                            | width = 20
                                            , widthSpec = SFixed 20
                                            , height = 20
                                            , heightSpec = SFixed 20
                                        }
                                    , AEl
                                        { default
                                            | width = 30
                                            , widthSpec = SFixed 30
                                            , height = 30
                                            , heightSpec = SFixed 30
                                        }
                                    ]
                            }
                        )
        , Test.test "Fit TB container with fixed children - width (across) filled in" <|
            \() ->
                Container
                    [ Width (Fit [])
                    , Height (Fit [])
                    , ChildGap 2
                    , Padding 5 5 5 5
                    , LayoutDirection TopToBottom
                    ]
                    [ Container [ Width (Fixed 10), Height (Fixed 10) ] []
                    , Container [ Width (Fixed 20), Height (Fixed 20) ] []
                    , Container [ Width (Fixed 30), Height (Fixed 30) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 40
                                , widthSpec = SFit
                                , height = 74
                                , heightSpec = SFit
                                , childGap = 2
                                , paddingLeft = 5
                                , paddingRight = 5
                                , paddingTop = 5
                                , paddingBottom = 5
                                , layoutDirection = TopToBottom
                                , children =
                                    [ AEl
                                        { default
                                            | width = 10
                                            , widthSpec = SFixed 10
                                            , height = 10
                                            , heightSpec = SFixed 10
                                        }
                                    , AEl
                                        { default
                                            | width = 20
                                            , widthSpec = SFixed 20
                                            , height = 20
                                            , heightSpec = SFixed 20
                                        }
                                    , AEl
                                        { default
                                            | width = 30
                                            , widthSpec = SFixed 30
                                            , height = 30
                                            , heightSpec = SFixed 30
                                        }
                                    ]
                            }
                        )
        , Test.test "fit parent of text should have the dimension filled" <|
            \() ->
                Container
                    [ LayoutDirection LeftToRight
                    , Width (Grow [])
                    ]
                    [ Container
                        [ Width (Grow []) ]
                        [ Text [] "Title" ]
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SGrow
                                , width = 640
                                , heightSpec = SFit
                                , height = 8
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SFit
                                            , width = 640
                                            , height = 8
                                            , children =
                                                [ AEl
                                                    { default
                                                        | widthSpec = SFit
                                                        , heightSpec = SFit
                                                        , width = 30
                                                        , height = 8
                                                        , text = Just "Title"
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        ]
