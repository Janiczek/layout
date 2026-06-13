module Step1FitSizingAlongTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Test exposing (Test)
import TestHelpers exposing (default)


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingAlong


fixedChild : Int -> Int -> AnnotatedEl
fixedChild width height =
    AEl
        { default
            | width = width
            , height = height
            , widthSpec = SFixed width
            , heightSpec = SFixed height
        }


suite : Test
suite =
    Test.describe "Step1.fitSizingAlong"
        [ -- TODO fuzz: sets fixed on both axes
          -- TODO fuzz: fit only sets along
          -- TODO fuzz: grow doesn't set anything
          -- TODO fuzz: padding
          -- TODO fuzz: child gap (fencepost rule)
          Test.test "default container -> still 0" <|
            \() ->
                Container [] []
                    |> run
                    |> Expect.equal (AEl default)
        , Test.test "fit container -> still 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    []
                    |> run
                    |> Expect.equal (AEl default)
        , Test.test "grow container -> still 0" <|
            \() ->
                Container
                    [ Height (Grow [])
                    , Width (Grow [])
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | heightSpec = SGrow
                                , widthSpec = SGrow
                            }
                        )
        , Test.test "fixed container LR -> both fixed dimensions filled" <|
            \() ->
                Container
                    [ Height (Fixed 123)
                    , Width (Fixed 456)
                    , LayoutDirection LeftToRight
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 456
                                , height = 123
                                , widthSpec = SFixed 456
                                , heightSpec = SFixed 123
                            }
                        )
        , Test.test "fit container LR with fixed child -> both same width, container unfilled height" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , LayoutDirection LeftToRight
                    ]
                    [ Container
                        [ Height (Fixed 123)
                        , Width (Fixed 456)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 456
                                , children = [ fixedChild 456 123 ]
                            }
                        )
        , Test.test "fit container with fit child -> both 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    [ Container
                        [ Height (Fit [])
                        , Width (Fit [])
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl { default | children = [ AEl default ] })
        , Test.test "fit container with padding -> use the padding" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , Padding 2 3 5 7
                    ]
                    []
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 3 + 7
                                , paddingTop = 2
                                , paddingRight = 3
                                , paddingBottom = 5
                                , paddingLeft = 7
                            }
                        )
        , Test.test "fit container with child, both with paddings -> use the padding" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , Padding 2 3 5 7
                    ]
                    [ Container [ Padding 20 30 50 70 ] [] ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 3 + 7 + 30 + 70
                                , paddingTop = 2
                                , paddingRight = 3
                                , paddingBottom = 5
                                , paddingLeft = 7
                                , children =
                                    [ AEl
                                        { default
                                            | width = 30 + 70
                                            , paddingTop = 20
                                            , paddingRight = 30
                                            , paddingBottom = 50
                                            , paddingLeft = 70
                                        }
                                    ]
                            }
                        )
        , Test.test "fit container with no children and gap -> don't use the gap" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    []
                    |> run
                    |> Expect.equal (AEl { default | childGap = 1 })
        , Test.test "fit container with one children and gap -> don't use the gap" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 200
                                , childGap = 1
                                , children = [ fixedChild 200 100 ]
                            }
                        )
        , Test.test "fit container with two children and gap -> use the gap 1x" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 401
                                , childGap = 1
                                , children =
                                    [ fixedChild 200 100
                                    , fixedChild 200 100
                                    ]
                            }
                        )
        , Test.test "fit container with two children and gap -> use the gap 1x, vertical" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    , LayoutDirection TopToBottom
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | height = 201
                                , layoutDirection = TopToBottom
                                , childGap = 1
                                , children =
                                    [ fixedChild 200 100
                                    , fixedChild 200 100
                                    ]
                            }
                        )
        , Test.test "fit container with three children and gap -> use the gap 2x" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    , ChildGap 1
                    ]
                    [ Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    , Container
                        [ Height (Fixed 100)
                        , Width (Fixed 200)
                        ]
                        []
                    ]
                    |> run
                    |> Expect.equal
                        (AEl
                            { default
                                | width = 602
                                , childGap = 1
                                , children =
                                    [ fixedChild 200 100
                                    , fixedChild 200 100
                                    , fixedChild 200 100
                                    ]
                            }
                        )
        ]
