module Step0AnnotateTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Test exposing (Test)
import TestHelpers


suite : Test
suite =
    Test.describe "Step0.annotate"
        [ Test.test "ex1 - purple root with no children" <|
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
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = Fit []
                            , widthSpec = Fit []
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = TopToBottom
                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                            }
                        )
        , Test.test "ex2 - purple menu with two light-purple items" <|
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
                                            , childGap = 0
                                            , fontSize = Just 18
                                            , heightSpec = Fit []
                                            , widthSpec = Fit []
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Just "Copy"
                                            , layoutDirection = LeftToRight
                                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                            }
                                        , AEl
                                            { position = { x = 0, y = 0 }
                                            , size = { width = 0, height = 0 }
                                            , bgColor = Nothing
                                            , children =
                                                [ AEl
                                                    { position = { x = 0, y = 0 }
                                                    , size = { width = 0, height = 0 }
                                                    , bgColor = Nothing
                                                    , children = []
                                                    , childGap = 0
                                                    , fontSize = Nothing
                                                    , heightSpec = Fit []
                                                    , widthSpec = Fit []
                                                    , horizAlign = HCenter
                                                    , vertAlign = VCenter
                                                    , text = Just "copy"
                                                    , layoutDirection = LeftToRight
                                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                                    }
                                                ]
                                            , childGap = 0
                                            , fontSize = Nothing
                                            , heightSpec = Fit []
                                            , widthSpec = Fit []
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Nothing
                                            , layoutDirection = LeftToRight
                                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                            }
                                        ]
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = Fit []
                                    , widthSpec = Grow []
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
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
                                            , childGap = 0
                                            , fontSize = Just 18
                                            , heightSpec = Fit []
                                            , widthSpec = Fit []
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Just "Paste"
                                            , layoutDirection = LeftToRight
                                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                            }
                                        , AEl
                                            { position = { x = 0, y = 0 }
                                            , size = { width = 0, height = 0 }
                                            , bgColor = Nothing
                                            , children =
                                                [ AEl
                                                    { position = { x = 0, y = 0 }
                                                    , size = { width = 0, height = 0 }
                                                    , bgColor = Nothing
                                                    , children = []
                                                    , childGap = 0
                                                    , fontSize = Nothing
                                                    , heightSpec = Fit []
                                                    , widthSpec = Fit []
                                                    , horizAlign = HCenter
                                                    , vertAlign = VCenter
                                                    , text = Just "paste"
                                                    , layoutDirection = LeftToRight
                                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                                    }
                                                ]
                                            , childGap = 0
                                            , fontSize = Nothing
                                            , heightSpec = Fit []
                                            , widthSpec = Fit []
                                            , horizAlign = HCenter
                                            , vertAlign = VCenter
                                            , text = Nothing
                                            , layoutDirection = LeftToRight
                                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                            }
                                        ]
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , heightSpec = Fit []
                                    , widthSpec = Grow []
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                    }
                                ]
                            , childGap = 0
                            , fontSize = Nothing
                            , heightSpec = Fit []
                            , widthSpec = Fit []
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = TopToBottom
                            , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                            }
                        )
        , Test.test "ex3 - blue container with pink and yellow children" <|
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
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , widthSpec = Fixed 300
                                    , heightSpec = Fixed 300
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                    }
                                , AEl
                                    { position = { x = 0, y = 0 }
                                    , size = { width = 0, height = 0 }
                                    , bgColor = Just Yellow
                                    , children = []
                                    , childGap = 0
                                    , fontSize = Nothing
                                    , widthSpec = Fixed 350
                                    , heightSpec = Fixed 200
                                    , horizAlign = HCenter
                                    , vertAlign = VCenter
                                    , text = Nothing
                                    , layoutDirection = LeftToRight
                                    , padding = { left = 0, right = 0, top = 0, bottom = 0 }
                                    }
                                ]
                            , childGap = 32
                            , fontSize = Nothing
                            , widthSpec = Fixed 960
                            , heightSpec = Fixed 540
                            , horizAlign = HCenter
                            , vertAlign = VCenter
                            , text = Nothing
                            , layoutDirection = LeftToRight
                            , padding = { left = 32, right = 32, top = 32, bottom = 32 }
                            }
                        )
        , Test.fuzz TestHelpers.el "position and size are zero" <|
            \input ->
                Step0.annotate input
                    |> annotatedPreOrder
                    |> List.all isZeroed
                    |> Expect.equal True

        -- TODO test that the attr properties are preserved?
        , Test.fuzz (Fuzz.list TestHelpers.el) "tree structure is preserved" <|
            \children ->
                let
                    el =
                        Container [] children
                in
                Step0.annotate el
                    |> childrenPreserved el
                    |> Expect.equal True
        ]


isZeroed : AnnotatedEl -> Bool
isZeroed annotated =
    case annotated of
        AEl { position, size } ->
            (position.x == 0)
                && (position.y == 0)
                && (size.width == 0)
                && (size.height == 0)


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
