module Step0AnnotateTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Test exposing (Test)
import TestHelpers exposing (default)


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
                    |> Expect.equal
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
                        [ Text [ FontSize 18 ] "Copy"
                        , Container [] [ Text [] "copy" ]
                        ]
                    , Container
                        [ Width (Grow [])
                        , BgColor LightPurple
                        ]
                        [ Text [ FontSize 18 ] "Paste"
                        , Container [] [ Text [] "paste" ]
                        ]
                    ]
                    |> Step0.annotate
                    |> Expect.equal
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
                                                [ AEl { default | fontSize = Just 18, text = Just "Copy" }
                                                , AEl
                                                    { default
                                                        | children =
                                                            [ AEl { default | text = Just "copy" } ]
                                                    }
                                                ]
                                        }
                                    , AEl
                                        { default
                                            | bgColor = Just LightPurple
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl { default | fontSize = Just 18, text = Just "Paste" }
                                                , AEl
                                                    { default
                                                        | children =
                                                            [ AEl { default | text = Just "paste" } ]
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
                    |> Expect.equal
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
                    |> List.all isZeroed
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
        ]


isZeroed : AnnotatedEl -> Bool
isZeroed annotated =
    case annotated of
        AEl { x, y, width, height } ->
            (x == 0)
                && (y == 0)
                && (width == 0)
                && (height == 0)


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
