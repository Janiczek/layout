module Step0AnnotateTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Test exposing (Test)
import TestHelpers exposing (default)
import Text


suite : Test
suite =
    Test.describe "Step0.annotate"
        [ Test.test "ex1 - purple root with no children" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , BgColor Purple
                    ]
                    []
                    |> Step0.annotate
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | bgColor = Just Purple
                                , layoutDirection = TopToBottom
                            }
                        )
        , Test.test "ex2 - purple menu with two light-purple items" <|
            \() ->
                Container
                    [ LayoutDirection TopToBottom
                    , BgColor Purple
                    ]
                    [ Container
                        [ Width (Grow [])
                        , BgColor LightPurple
                        ]
                        [ Text [ FontColor Blue ] "Copy"
                        , Container [] [ Text [] "copy" ]
                        ]
                    , Container
                        [ Width (Grow [])
                        , BgColor LightPurple
                        ]
                        [ Text [ FontColor Pink ] "Paste"
                        , Container [] [ Text [] "paste" ]
                        ]
                    ]
                    |> Step0.annotate
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | bgColor = Just Purple
                                , layoutDirection = TopToBottom
                                , children =
                                    [ AEl
                                        { default
                                            | bgColor = Just LightPurple
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl
                                                    { default
                                                        | fontColor = Just Blue
                                                        , text = Just "Copy"
                                                        , width = 4 * Text.charWidth
                                                        , height = 1 * Text.charHeight
                                                    }
                                                , AEl
                                                    { default
                                                        | children =
                                                            [ AEl
                                                                { default
                                                                    | text = Just "copy"
                                                                    , width = 4 * Text.charWidth
                                                                    , height = 1 * Text.charHeight
                                                                }
                                                            ]
                                                    }
                                                ]
                                        }
                                    , AEl
                                        { default
                                            | bgColor = Just LightPurple
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl
                                                    { default
                                                        | fontColor = Just Pink
                                                        , text = Just "Paste"
                                                        , width = 5 * Text.charWidth
                                                        , height = 1 * Text.charHeight
                                                    }
                                                , AEl
                                                    { default
                                                        | children =
                                                            [ AEl
                                                                { default
                                                                    | text = Just "paste"
                                                                    , width = 5 * Text.charWidth
                                                                    , height = 1 * Text.charHeight
                                                                }
                                                            ]
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        , Test.test "ex3 - blue container with pink and yellow children" <|
            \() ->
                Container
                    [ Width (Fixed 960)
                    , Height (Fixed 540)
                    , BgColor Blue
                    , Padding 32 32 32 32
                    , ChildGap 32
                    ]
                    [ Container
                        [ Width (Fixed 300)
                        , Height (Fixed 300)
                        , BgColor Pink
                        ]
                        []
                    , Container
                        [ Width (Fixed 350)
                        , Height (Fixed 200)
                        , BgColor Yellow
                        ]
                        []
                    ]
                    |> Step0.annotate
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | bgColor = Just Blue
                                , childGap = 32
                                , widthSpec = SFixed 960
                                , heightSpec = SFixed 540
                                , paddingTop = 32
                                , paddingRight = 32
                                , paddingBottom = 32
                                , paddingLeft = 32
                                , children =
                                    [ AEl
                                        { default
                                            | bgColor = Just Pink
                                            , widthSpec = SFixed 300
                                            , heightSpec = SFixed 300
                                        }
                                    , AEl
                                        { default
                                            | bgColor = Just Yellow
                                            , widthSpec = SFixed 350
                                            , heightSpec = SFixed 200
                                        }
                                    ]
                            }
                        )
        , Test.fuzz TestHelpers.el "position and size are zero" <|
            \input ->
                input
                    |> Step0.annotate
                    |> El.preOrder
                    |> List.all nonTextIsZeroed
                    |> Expect.equal True

        -- TODO properties are preserved (except for padding, childGap which are non-negative)
        , Test.fuzz (Fuzz.list TestHelpers.el) "tree structure is preserved" <|
            \children ->
                let
                    el =
                        Container [] children
                in
                el
                    |> Step0.annotate
                    |> childrenPreserved el
                    |> Expect.equal True
        , Test.test "Single-line text is measured" <|
            \() ->
                Text [] "Abcde"
                    |> Step0.annotate
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 5 * Text.charWidth
                                , height = 1 * Text.charHeight
                                , text = Just "Abcde"
                            }
                        )
        , Test.test "Multi-line text is measured" <|
            \() ->
                Text [] "Abcde\nxyz\n\nDEFGHIJKL"
                    |> Step0.annotate
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 9 * Text.charWidth
                                , height = 4 * Text.charHeight
                                , text = Just "Abcde\nxyz\n\nDEFGHIJKL"
                            }
                        )
        ]


nonTextIsZeroed : AnnotatedEl -> Bool
nonTextIsZeroed (AEl { text, x, y, width, height }) =
    case text of
        Nothing ->
            (x == 0)
                && (y == 0)
                && (width == 0)
                && (height == 0)

        Just _ ->
            True


childrenPreserved : El -> AnnotatedEl -> Bool
childrenPreserved el annotated =
    case ( el, annotated ) of
        ( Container _ elChildren, AEl { children } ) ->
            (List.length elChildren == List.length children)
                && (List.map2 childrenPreserved elChildren children
                        |> List.all identity
                   )

        ( Text _ _, AEl { children } ) ->
            List.isEmpty children

        ( Empty, AEl { children } ) ->
            List.isEmpty children
