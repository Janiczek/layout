module Step0AnnotateTests exposing (suite)

import El exposing (..)
import Expect
import Step0Annotate as Step0
import Test exposing (Test)
import TestHelpers


suite : Test
suite =
    Test.describe "Step0.annotate"
        [ Test.describe "ex1"
            [ Test.test "purple root with no children" <|
                \() ->
                    Step0.annotate
                        (Container
                            [ LayoutDirection TopToBottom
                            , BgColor Purple
                            ]
                            []
                        )
                        |> Expect.equal
                            (AEl
                                { position = { x = 0, y = 0 }
                                , size = { width = 0, height = 0 }
                                , bgColor = Just Purple
                                , children = []
                                }
                            )
            ]
        , Test.describe "ex2"
            [ Test.test "purple menu with two light-purple items" <|
                \() ->
                    Step0.annotate
                        (Container
                            [ LayoutDirection TopToBottom
                            , BgColor Purple
                            ]
                            [ Container
                                [ Width (Grow [])
                                , BgColor LightPurple
                                ]
                                [ Text [ FontSize 18 ] "Copy"
                                , Container [ ImageData "copy" ] []
                                ]
                            , Container
                                [ Width (Grow [])
                                , BgColor LightPurple
                                ]
                                [ Text [ FontSize 18 ] "Paste"
                                , Container [ ImageData "paste" ] []
                                ]
                            ]
                        )
                        |> Expect.equal
                            (AEl
                                { position = { x = 0, y = 0 }
                                , size = { width = 0, height = 0 }
                                , bgColor = Just Purple
                                , children =
                                    [ AEl
                                        { position = { x = 0, y = 0 }
                                        , size = { width = 0, height = 0 }
                                        , bgColor = Just LightPurple
                                        , children =
                                            [ AEl
                                                { position = { x = 0, y = 0 }
                                                , size = { width = 0, height = 0 }
                                                , bgColor = Nothing
                                                , children = []
                                                }
                                            , AEl
                                                { position = { x = 0, y = 0 }
                                                , size = { width = 0, height = 0 }
                                                , bgColor = Nothing
                                                , children = []
                                                }
                                            ]
                                        }
                                    , AEl
                                        { position = { x = 0, y = 0 }
                                        , size = { width = 0, height = 0 }
                                        , bgColor = Just LightPurple
                                        , children =
                                            [ AEl
                                                { position = { x = 0, y = 0 }
                                                , size = { width = 0, height = 0 }
                                                , bgColor = Nothing
                                                , children = []
                                                }
                                            , AEl
                                                { position = { x = 0, y = 0 }
                                                , size = { width = 0, height = 0 }
                                                , bgColor = Nothing
                                                , children = []
                                                }
                                            ]
                                        }
                                    ]
                                }
                            )
            ]
        , Test.describe "ex3"
            [ Test.test "blue container with pink and yellow children" <|
                \() ->
                    Step0.annotate
                        (Container
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
                        )
                        |> Expect.equal
                            (AEl
                                { position = { x = 0, y = 0 }
                                , size = { width = 0, height = 0 }
                                , bgColor = Just Blue
                                , children =
                                    [ AEl
                                        { position = { x = 0, y = 0 }
                                        , size = { width = 0, height = 0 }
                                        , bgColor = Just Pink
                                        , children = []
                                        }
                                    , AEl
                                        { position = { x = 0, y = 0 }
                                        , size = { width = 0, height = 0 }
                                        , bgColor = Just Yellow
                                        , children = []
                                        }
                                    ]
                                }
                            )
            ]
        , Test.test "initializes position and size to zero" <|
            \() ->
                case
                    Step0.annotate
                        (Container
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
                        )
                of
                    AEl { position, size, children } ->
                        case children of
                            AEl child :: _ ->
                                Expect.all
                                    [ \_ -> Expect.equal 0 position.x
                                    , \_ -> Expect.equal 0 position.y
                                    , \_ -> Expect.equal 0 size.width
                                    , \_ -> Expect.equal 0 size.height
                                    , \_ -> Expect.equal 0 child.size.width
                                    , \_ -> Expect.equal 0 child.size.height
                                    ]
                                    ()

                            [] ->
                                Expect.fail "expected children"
        , Test.describe "fuzz"
            [ Test.fuzz TestHelpers.el "position and size are zero" <|
                \input ->
                    Step0.annotate input
                        |> annotatedPreOrder
                        |> List.all isZeroed
                        |> Expect.equal True
            , Test.fuzz TestHelpers.el "bgColor is preserved" <|
                \input ->
                    let
                        annotated =
                            Step0.annotate input
                    in
                    List.map2
                        (\el (AEl { bgColor }) -> TestHelpers.bgColorFromEl el == bgColor)
                        (foldPreOrder (::) [] input |> List.reverse)
                        (annotatedPreOrder annotated)
                        |> List.all identity
                        |> Expect.equal True
            , Test.fuzz TestHelpers.el "children list is preserved" <|
                \input ->
                    Step0.annotate input
                        |> childrenPreserved input
                        |> Expect.equal True
            ]
        ]


isZeroed : AnnotatedEl -> Bool
isZeroed annotated =
    case annotated of
        AEl { position, size } ->
            position.x
                == 0
                && position.y
                == 0
                && size.width
                == 0
                && size.height
                == 0


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
