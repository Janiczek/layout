module LayoutTests exposing (suite)

import El exposing (..)
import Example
import Expect
import Fuzz
import Layout
import List.Extra
import Test exposing (Test)
import TestHelpers exposing (default)
import Text


config : Config
config =
    { layoutWidth = 640
    , layoutHeight = 480
    }


overlaps :
    { a | left : number, right : number, top : number, bottom : number }
    -> { a | left : number, right : number, top : number, bottom : number }
    -> Bool
overlaps c1 c2 =
    not
        ((c2.left >= c1.right)
            || (c2.right <= c1.left)
            || (c2.top >= c1.bottom)
            || (c2.bottom <= c1.top)
        )


suite : Test
suite =
    Test.describe "Layout.layout (end-to-end)"
        [ Test.fuzz TestHelpers.el "No two siblings overlap" <|
            \el ->
                let
                    overlappingSiblings : List ( String, String )
                    overlappingSiblings =
                        el
                            |> Layout.layout config
                            |> El.preOrder
                            |> List.concatMap
                                (\((AEl parent) as parent_) ->
                                    parent.children
                                        |> List.map
                                            (\((AEl child) as child_) ->
                                                { left = child.x
                                                , top = child.y
                                                , right = child.x + child.width
                                                , bottom = child.y + child.height
                                                , width = child.width
                                                , height = child.height
                                                , ael = child_
                                                , parent = parent_
                                                }
                                            )
                                        |> List.Extra.uniquePairs
                                        |> List.filterMap
                                            (\( c1, c2 ) ->
                                                if
                                                    (c1.width /= 0)
                                                        && (c1.height /= 0)
                                                        && (c2.width /= 0)
                                                        && (c2.height /= 0)
                                                        && overlaps c1 c2
                                                then
                                                    Just
                                                        ( El.printout c1.ael
                                                        , El.printout c2.ael
                                                        )

                                                else
                                                    Nothing
                                            )
                                )
                in
                (overlappingSiblings == [])
                    |> Expect.equal True
                    |> Expect.onFail
                        ("Overlapping siblings:\n\n"
                            ++ (overlappingSiblings
                                    |> List.map
                                        (\( parent, child ) ->
                                            "Sibling 1:\n"
                                                ++ parent
                                                ++ "\n\nSibling 2:\n"
                                                ++ child
                                        )
                                    |> String.join "\n\n"
                               )
                        )
        , Test.fuzz TestHelpers.el "No negative width,height" <|
            \el ->
                let
                    laidout : AnnotatedEl
                    laidout =
                        el
                            |> Layout.layout config

                    nodesWithNegativeSize : List String
                    nodesWithNegativeSize =
                        laidout
                            |> El.preOrder
                            |> List.filterMap
                                (\((AEl node) as node_) ->
                                    if (node.width < 0) || (node.height < 0) then
                                        Just (El.printout node_)

                                    else
                                        Nothing
                                )
                in
                (nodesWithNegativeSize == [])
                    |> Expect.equal True
                    |> Expect.onFail
                        ("Nodes with negative size:\n\n"
                            ++ (nodesWithNegativeSize
                                    |> List.map
                                        (\node ->
                                            "Node:\n"
                                                ++ node
                                        )
                                    |> String.join "\n\n"
                               )
                            ++ "\n\nThe whole tree:\n\n"
                            ++ El.printout laidout
                        )
        , Test.fuzz
            (Fuzz.asciiStringOfLengthBetween 1 10
                |> Fuzz.map
                    (String.map
                        (\c ->
                            if c == ' ' then
                                'X'

                            else
                                c
                        )
                    )
            )
            "Text dimensions non-zero"
          <|
            \text ->
                let
                    ((AEl final) as final_) =
                        Text [] text
                            |> Layout.layout config
                in
                final
                    |> Expect.all
                        [ .width >> Expect.greaterThan 0
                        , .height >> Expect.greaterThan 0
                        ]
                    |> Expect.onFail (El.printout final_)
        , Test.test "Basic layout - holy grail" <|
            -- TODO: Doesn't test shrinking or text wrapping
            \() ->
                -- root
                Container
                    [ LayoutDirection TopToBottom
                    , Width (Grow [])
                    , Height (Grow [])
                    ]
                    [ -- header
                      Container
                        [ LayoutDirection LeftToRight
                        , Width (Grow [])
                        , ChildGap 16
                        , Padding 8 8 8 8
                        , BgColor Purple
                        ]
                        [ -- title
                          Container
                            [ Width (Grow []) ]
                            [ Text [] "Title" ]
                        , -- login
                          Text [] "Login"
                        ]
                    , -- main
                      Container
                        [ LayoutDirection LeftToRight
                        , Width (Grow [])
                        , Height (Grow [])
                        ]
                        [ -- left rail
                          Container
                            [ LayoutDirection TopToBottom
                            , Height (Grow [])
                            , Padding 8 8 8 8
                            , ChildGap 16
                            , BgColor LightPurple
                            ]
                            [ -- Top section of left rail
                              Container
                                [ Height (Grow [])
                                , ChildGap 8
                                ]
                                [ Text [] "Home"
                                , Text [] "About me"
                                , Text [] "Downloads"
                                ]
                            , -- Bottom section of left rail
                              Container []
                                [ Text [] "Admin link"
                                ]
                            ]
                        , -- content
                          Container
                            [ LayoutDirection TopToBottom
                            , Width (Grow [])
                            , Height (Grow [])
                            , BgColor Pink
                            , ChildGap 8
                            , Padding 8 8 8 8
                            ]
                            [ Text [] "Heading!"
                            , Text [] "Some text"
                            , Text [] "Some more text here."
                            ]
                        , -- right rail
                          Container
                            [ LayoutDirection TopToBottom
                            , Height (Grow [])
                            , BgColor LightPurple
                            , ChildGap 8
                            , Padding 8 8 8 8
                            ]
                            [ Text [] "On this page"
                            , Text [] "Tags"
                            , Text [] "Reading time"
                            ]
                        ]
                    , -- footer
                      Container
                        [ Width (Grow [])
                        , BgColor Purple
                        ]
                        [ Text [] "(c) 2026 MJ" ]
                    ]
                    |> Layout.layout config
                    |> TestHelpers.expectEqualAnnotatedEl
                        -- root
                        (AEl
                            { default
                                | width = 640
                                , height = 480
                                , layoutDirection = TopToBottom
                                , widthSpec = SGrow
                                , heightSpec = SGrow
                                , children =
                                    [ -- header
                                      AEl
                                        { default
                                            | width = 640
                                            , height = 24
                                            , widthSpec = SGrow
                                            , paddingTop = 8
                                            , paddingRight = 8
                                            , paddingBottom = 8
                                            , paddingLeft = 8
                                            , childGap = 16
                                            , bgColor = Just Purple
                                            , children =
                                                [ -- title
                                                  AEl
                                                    { default
                                                        | widthSpec = SGrow
                                                        , width = 578
                                                        , height = 1 * Text.charHeight
                                                        , x = 8
                                                        , y = 8
                                                        , children =
                                                            [ AEl
                                                                { default
                                                                    | width = 5 * Text.charWidth
                                                                    , height = 1 * Text.charHeight
                                                                    , x = 8
                                                                    , y = 8
                                                                    , text = Just "Title"
                                                                }
                                                            ]
                                                    }
                                                , -- login
                                                  AEl
                                                    { default
                                                        | width = 5 * Text.charWidth
                                                        , height = 1 * Text.charHeight
                                                        , x = 602
                                                        , y = 8
                                                        , text = Just "Login"
                                                    }
                                                ]
                                        }
                                    , -- main
                                      AEl
                                        { default
                                            | widthSpec = SGrow
                                            , heightSpec = SGrow
                                            , children =
                                                [ -- left rail
                                                  AEl
                                                    { default
                                                        | layoutDirection = TopToBottom
                                                        , heightSpec = SGrow
                                                        , paddingTop = 8
                                                        , paddingRight = 8
                                                        , paddingBottom = 8
                                                        , paddingLeft = 8
                                                        , childGap = 16
                                                        , bgColor = Just LightPurple
                                                        , children =
                                                            [ -- Top section of left rail
                                                              AEl
                                                                { default
                                                                    | heightSpec = SGrow
                                                                    , childGap = 8
                                                                    , children =
                                                                        [ AEl { default | text = Just "Home" }
                                                                        , AEl { default | text = Just "About me" }
                                                                        , AEl { default | text = Just "Downloads" }
                                                                        ]
                                                                }
                                                            , -- Bottom section of left rail
                                                              AEl
                                                                { default
                                                                    | children =
                                                                        [ AEl { default | text = Just "Admin link" }
                                                                        ]
                                                                }
                                                            ]
                                                    }
                                                , -- content
                                                  AEl
                                                    { default
                                                        | layoutDirection = TopToBottom
                                                        , heightSpec = SGrow
                                                        , widthSpec = SGrow
                                                        , paddingTop = 8
                                                        , paddingRight = 8
                                                        , paddingBottom = 8
                                                        , paddingLeft = 8
                                                        , childGap = 8
                                                        , bgColor = Just Pink
                                                        , children =
                                                            [ AEl { default | text = Just "Heading!" }
                                                            , AEl { default | text = Just "Some text" }
                                                            , AEl { default | text = Just "Some more text here." }
                                                            ]
                                                    }
                                                , -- right rail
                                                  AEl
                                                    { default
                                                        | heightSpec = SGrow
                                                        , layoutDirection = TopToBottom
                                                        , childGap = 8
                                                        , paddingTop = 8
                                                        , paddingRight = 8
                                                        , paddingBottom = 8
                                                        , paddingLeft = 8
                                                        , bgColor = Just LightPurple
                                                        , children =
                                                            [ AEl { default | text = Just "On this page" }
                                                            , AEl { default | text = Just "Tags" }
                                                            , AEl { default | text = Just "Reading time" }
                                                            ]
                                                    }
                                                ]
                                        }
                                    , -- footer
                                      AEl
                                        { default
                                            | width = 640
                                            , height = 8
                                            , x = 0

                                            -- , y = 504
                                            , bgColor = Just Purple
                                            , widthSpec = SGrow
                                            , children =
                                                [ AEl
                                                    { default
                                                        | width = 11 * Text.charWidth
                                                        , height = 1 * Text.charHeight
                                                        , text = Just "(c) 2026 MJ"
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )
        ]
