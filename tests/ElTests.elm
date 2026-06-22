module ElTests exposing (suite)

import El exposing (..)
import Expect exposing (Expectation)
import Fuzz
import List.Extra
import Step0Annotate as Step0
import Test exposing (Test)


at : Int -> (AnnotatedElData -> Expectation) -> List AnnotatedEl -> Expectation
at index fn all =
    case List.Extra.getAt index all of
        Nothing ->
            Expect.fail "Out of bounds"

        Just (AEl ael) ->
            fn ael


suite : Test
suite =
    Test.describe "El"
        [ Test.describe "preOrder"
            [ Test.test "correct order" <|
                \() ->
                    Container [ BgColor Pink ]
                        [ Container [ BgColor Black ]
                            [ Text [] "A"
                            ]
                        , Text [] "B"
                        ]
                        |> Step0.annotate
                        |> El.preOrder
                        |> Expect.all
                            [ at 0 (.bgColor >> Expect.equal (Just Pink))
                            , at 1 (.bgColor >> Expect.equal (Just Black))
                            , at 2 (.text >> Expect.equal (Just "A"))
                            , at 3 (.text >> Expect.equal (Just "B"))
                            ]
            ]
        , Test.describe "postOrder"
            [ Test.test "correct order" <|
                \() ->
                    Container [ BgColor Pink ]
                        [ Container [ BgColor Black ]
                            [ Text [] "A"
                            ]
                        , Text [] "B"
                        ]
                        |> Step0.annotate
                        |> El.postOrder
                        |> Expect.all
                            [ at 0 (.text >> Expect.equal (Just "A"))
                            , at 1 (.bgColor >> Expect.equal (Just Black))
                            , at 2 (.text >> Expect.equal (Just "B"))
                            , at 3 (.bgColor >> Expect.equal (Just Pink))
                            ]
            ]
        ]
