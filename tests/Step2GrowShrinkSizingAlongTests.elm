module Step2GrowShrinkSizingAlongTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Step2GrowShrinkSizingAlong as Step2
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


suite : Test
suite =
    Test.describe "Step1.fitSizingAlong"
        [ Test.test "default container -> still 0" <|
            \() ->
                Container [] []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl (AEl default)
        , Test.test "fit container -> still 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl (AEl default)
        , Test.test "grow root -> takes layout size - LR" <|
            \() ->
                Container
                    [ Height (Grow [])
                    , Width (Grow [])
                    , LayoutDirection LeftToRight
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | heightSpec = SGrow
                                , widthSpec = SGrow
                                , width = 640
                            }
                        )
        , Test.test "grow root -> takes layout size - TB" <|
            \() ->
                Container
                    [ Height (Grow [])
                    , Width (Grow [])
                    , LayoutDirection TopToBottom
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | heightSpec = SGrow
                                , widthSpec = SGrow
                                , layoutDirection = TopToBottom
                                , height = 480
                            }
                        )
        , Test.test "grow child -> takes parent size - LR" <|
            \() ->
                Container
                    [ Height (Fixed 300)
                    , Width (Fixed 200)
                    ]
                    [ Container
                        [ Width (Grow [])
                        , LayoutDirection LeftToRight
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
                                , height = 300
                                , children =
                                    [ AEl
                                        { default
                                            | widthSpec = SGrow
                                            , width = 200
                                        }
                                    ]
                            }
                        )
        , Test.test "grow child -> takes parent size - TB" <|
            \() ->
                Container
                    [ Height (Fixed 300)
                    , Width (Fixed 200)
                    ]
                    [ Container
                        [ Height (Grow [])
                        , LayoutDirection TopToBottom
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
                                , height = 300
                                , children =
                                    [ AEl
                                        { default
                                            | heightSpec = SGrow
                                            , height = 300
                                            , layoutDirection = TopToBottom
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
                                            , heightSpec = SFixed 200
                                            , width = 872
                                            , height = 200
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
        ]
