module Step3WrapTextTests exposing (suite)

import El exposing (..)
import Expect
import Fuzz
import Step0Annotate as Step0
import Step1FitSizingAlong as Step1
import Step2GrowShrinkSizingAlong as Step2
import Step3WrapText as Step3 exposing (charHeight, charWidth)
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


suite : Test
suite =
    Test.describe "Step3.wrapText"
        [ Test.test "Short text without newlines in Fit container doesn't wrap" <|
            \() ->
                Container
                    [ Width (Fit []) ]
                    [ Text [] "Hello" ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = 5 * charWidth
                                , height = 0
                                , children =
                                    [ AEl
                                        { default
                                            | width = 5 * charWidth
                                            , height = 1 * charHeight
                                            , text = Just "Hello"
                                        }
                                    ]
                            }
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
                            |> run
                in
                final
                    |> Expect.all
                        [ .width >> Expect.greaterThan 0
                        , .height >> Expect.greaterThan 0
                        ]
                    |> Expect.onFail (El.printout final_)
        , Test.test "Short text without newlines in Fixed container doesn't wrap" <|
            \() ->
                Container
                    [ Width (Fixed 100) ]
                    [ Text [] "Hello" ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed 100
                                , width = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 5 * charWidth
                                            , height = 1 * charHeight
                                            , text = Just "Hello"
                                        }
                                    ]
                            }
                        )
        , Test.test "Text without newlines in Fit container doesn't wrap" <|
            \() ->
                let
                    text =
                        "Hello Hello Hello Hello Hello Hello Hello Hello Hello"
                in
                Container
                    [ Width (Fit []) ]
                    [ Text [] text ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | width = String.length text * charWidth
                                , children =
                                    [ AEl
                                        { default
                                            | width = String.length text * charWidth
                                            , height = 1 * charHeight
                                            , text = Just text
                                        }
                                    ]
                            }
                        )
        , Test.test "Text without newlines in Fixed container wraps due to width" <|
            \() ->
                let
                    text =
                        "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello"
                in
                Container
                    [ Width (Fixed (60 * charWidth)) ]
                    [ Text [] text ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed (60 * charWidth)
                                , width = 60 * charWidth
                                , children =
                                    [ AEl
                                        { default
                                            | width = 60 * charWidth
                                            , height = 2 * charHeight
                                            , text =
                                                [ "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello "
                                                , "Hello Hello Hello Hello Hello"
                                                ]
                                                    |> String.join "\n"
                                                    |> Just
                                        }
                                    ]
                            }
                        )
        , Test.test "Wrapping doesn't care about being inside a word" <|
            \() ->
                let
                    text =
                        "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello"
                in
                Container
                    [ Width (Fixed (61 * charWidth)) ]
                    [ Text [] text ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed (61 * charWidth)
                                , width = 61 * charWidth
                                , children =
                                    [ AEl
                                        { default
                                            | width = 61 * charWidth
                                            , height = 2 * charHeight
                                            , text =
                                                [ "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello H"
                                                , "ello Hello Hello Hello Hello"
                                                ]
                                                    |> String.join "\n"
                                                    |> Just
                                        }
                                    ]
                            }
                        )
        , Test.test "Text with newlines in Fit container wraps only those existing newlines" <|
            \() ->
                let
                    text =
                        -- lines: 59, 11, 35 characters
                        [ "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello"
                        , "Hello Hello"
                        , "Hello Hello Hello Hello Hello Hello"
                        ]
                            |> String.join "\n"
                in
                Container
                    [ Width (Fit []) ]
                    [ Text [] text ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFit
                                , width = 59 * charWidth
                                , height = 0
                                , children =
                                    [ AEl
                                        { default
                                            | width = 59 * charWidth
                                            , height = 3 * charHeight
                                            , text = Just text
                                        }
                                    ]
                            }
                        )
        , Test.test "Text with newlines in Fixed container wraps due to width and existing newlines" <|
            \() ->
                let
                    text =
                        -- lines: 59, 11, 35 characters
                        [ "Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello"
                        , "Hello Hello"
                        , "Hello Hello Hello Hello Hello Hello"
                        ]
                            |> String.join "\n"
                in
                Container
                    [ Width (Fixed (31 * charWidth)) ]
                    [ Text [] text ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SFixed (31 * charWidth)
                                , width = 31 * charWidth
                                , children =
                                    [ AEl
                                        { default
                                            | width = 31 * charWidth
                                            , height = 5 * charHeight
                                            , text =
                                                [ "Hello Hello Hello Hello Hello H"
                                                , "ello Hello Hello Hello Hello"
                                                , "Hello Hello"
                                                , "Hello Hello Hello Hello Hello H"
                                                , "ello"
                                                ]
                                                    |> String.join "\n"
                                                    |> Just
                                        }
                                    ]
                            }
                        )
        , Test.test "Text sizes its parent Fit container along the axis as well" <|
            \() ->
                Container
                    [ Width (Grow [])
                    , Height (Fixed 100)
                    ]
                    [ Container
                        [ Width (Fit [])
                        , Height (Fit [])
                        ]
                        [ Text [] "Hello" ]
                    ]
                    |> run
                    |> TestHelpers.expectEqualAnnotatedEl
                        (AEl
                            { default
                                | widthSpec = SGrow
                                , heightSpec = SFixed 100
                                , width = 640
                                , height = 100
                                , children =
                                    [ AEl
                                        { default
                                            | width = 5 * charWidth
                                            , height = 0
                                            , children =
                                                [ AEl
                                                    { default
                                                        | width = 5 * charWidth
                                                        , height = 1 * charHeight
                                                        , text = Just "Hello"
                                                    }
                                                ]
                                        }
                                    ]
                            }
                        )

        -- TODO spaces at the end - does it cause wrap? or does it get trimmed
        -- TODO newlines at the end - does it cause wrap? or does it get trimmed
        -- TODO Grow container without newlines
        -- TODO Grow container with newlines
        -- TODO fuzz: non-text elements behave the same as if you only ran steps 0-2
        ]
