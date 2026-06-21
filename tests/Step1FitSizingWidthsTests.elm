module Step1FitSizingWidthsTests exposing (suite)

import El exposing (..)
import Expect exposing (Expectation)
import Fuzz
import Step0Annotate as Step0
import Step1FitSizingWidths as Step1
import Test exposing (Test)
import TestHelpers exposing (default)


run : El -> AnnotatedEl
run el =
    el
        |> Step0.annotate
        |> Step1.fitSizingWidths


suite : Test
suite =
    Test.describe "Step1.fitSizingWidths"
        [ Test.test "default container -> still 0" <|
            \() ->
                Container [] []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 0
                            }
                        )
        , Test.test "fit container -> still 0" <|
            \() ->
                Container
                    [ Height (Fit [])
                    , Width (Fit [])
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 0
                            }
                        )
        , Test.test "grow container -> still 0" <|
            \() ->
                Container
                    [ Width (Grow [])
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SGrow
                                , width = 0
                            }
                        )
        , Test.test "fixed container LR -> width filled" <|
            \() ->
                Container
                    [ Height (Fixed 123)
                    , Width (Fixed 456)
                    , LayoutDirection LeftToRight
                    ]
                    []
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 456
                                , height = 0
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
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 456
                                , height = 0
                                , children =
                                    [ AEl
                                        { default
                                            | width = 456
                                            , height = 0
                                            , widthSpec = SFixed 456
                                            , heightSpec = SFixed 123
                                        }
                                    ]
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
                    |> TestHelpers.expectEqualAnnotatedEl
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
                    |> TestHelpers.expectEqualAnnotatedEl
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
                    |> TestHelpers.expectEqualAnnotatedEl
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
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl { default | childGap = 1 })
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
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 200
                                , height = 0
                                , childGap = 1
                                , children =
                                    [ AEl
                                        { default
                                            | width = 200
                                            , height = 0
                                            , widthSpec = SFixed 200
                                            , heightSpec = SFixed 100
                                        }
                                    ]
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
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 401
                                , height = 0
                                , childGap = 1
                                , children =
                                    [ AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    , AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    ]
                            }
                        )
        , Test.test "fit TB container with two children and gap -> nothing done about height in this step" <|
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
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | height = 0
                                , width = 200
                                , layoutDirection = TopToBottom
                                , childGap = 1
                                , children =
                                    [ AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    , AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
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
                    [ Container [ Height (Fixed 100), Width (Fixed 200) ] []
                    , Container [ Height (Fixed 100), Width (Fixed 200) ] []
                    , Container [ Height (Fixed 100), Width (Fixed 200) ] []
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 602
                                , height = 0
                                , childGap = 1
                                , children =
                                    [ AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    , AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    , AEl { default | width = 200, height = 0, widthSpec = SFixed 200, heightSpec = SFixed 100 }
                                    ]
                            }
                        )
        , Test.fuzz TestHelpers.el "non-text height always 0" <|
            \el ->
                let
                    default : () -> Expectation
                    default () =
                        el
                            |> run
                            |> Expect.all
                                [ El.inner .height >> Expect.equal 0
                                ]
                in
                case el of
                    Text _ _ ->
                        Expect.pass

                    -- text can have height >0
                    Empty ->
                        default ()

                    Container _ _ ->
                        default ()
        , Test.fuzz3 TestHelpers.layoutDirectionFuzzer Fuzz.int Fuzz.int "fit -> set only width" <|
            \ld w h ->
                Container
                    [ Width (Fit [])
                    , Height (Fit [])
                    , LayoutDirection ld
                    ]
                    [ Container
                        [ Width (Fixed w)
                        , Height (Fixed h)
                        ]
                        []
                    ]
                    |> run
                    |> rootWidthAndHeight
                    |> Expect.equal ( max 0 w, 0 )
        , Test.fuzz TestHelpers.el "fit - parent.width >= child.width" <|
            \el ->
                let
                    badRootsAndChildren : List ( String, String )
                    badRootsAndChildren =
                        el
                            |> run
                            |> El.postOrder
                            |> List.concatMap
                                (\((AEl root) as root_) ->
                                    root.children
                                        |> List.filterMap
                                            (\((AEl child) as child_) ->
                                                if
                                                    (root.widthSpec /= SFit)
                                                        || (root.width >= child.width)
                                                then
                                                    Nothing

                                                else
                                                    Just ( El.printout root_, El.printout child_ )
                                            )
                                )
                in
                (badRootsAndChildren == [])
                    |> Expect.equal True
                    |> Expect.onFail
                        ("Bad parents and children:\n\n"
                            ++ (badRootsAndChildren
                                    |> List.map (\( parent, child ) -> "Parent:\n" ++ parent ++ "\n\nChild:\n" ++ child)
                                    |> String.join "\n\n"
                               )
                        )

        -- TODO fuzz: grow doesn't set anything
        -- TODO fuzz: padding
        -- TODO fuzz: child gap (fencepost rule)
        -- TODO fuzz and unit: Fit with min()
        -- TODO fuzz and unit: Fit with max()
        -- TODO fuzz and unit: Fit with min(), max()
        -- TODO fuzz and unit: Grow with min()
        -- TODO fuzz and unit: Grow with max()
        -- TODO fuzz and unit: Grow with min(), max()
        ]


rootWidthAndHeight : AnnotatedEl -> ( Int, Int )
rootWidthAndHeight (AEl ael) =
    ( ael.width, ael.height )
