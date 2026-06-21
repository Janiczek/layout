module LayoutTests exposing (suite)

import El exposing (..)
import Example
import Expect
import Fuzz
import Layout
import List.Extra
import Test exposing (Test)
import TestHelpers exposing (default)


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
                    nodesWithNegativeSize : List String
                    nodesWithNegativeSize =
                        el
                            |> Layout.layout config
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
        , Test.skip <|
            Test.test "Basic layout - holy grail" <|
                \() ->
                    Example.holyGrail
                        |> Layout.layout config
                        |> TestHelpers.expectEqualAnnotatedEl
                            (Debug.todo "holy grail annotated")
        ]
