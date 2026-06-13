module TestHelpers exposing (default, el, elWithMaxDepth, expectEqualAnnotatedEl)

import El exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)


expectEqualAnnotatedEl : AnnotatedEl -> AnnotatedEl -> Expectation
expectEqualAnnotatedEl expected actual =
    actual
        |> Expect.equal expected
        |> Expect.onFail
            ([ "AnnotatedEls not equal:"
             , "expected"
             , El.printout actual
             , "to equal"
             , El.printout expected
             ]
                |> String.join "\n\n"
            )


default : AnnotatedElData
default =
    El.empty


el : Fuzzer El
el =
    elWithMaxDepth 4


elWithMaxDepth : Int -> Fuzzer El
elWithMaxDepth depth =
    if depth <= 0 then
        Fuzz.oneOf
            [ textEl
            , emptyEl
            , containerElWithoutChildren
            ]

    else
        Fuzz.oneOf
            [ textEl
            , emptyEl
            , containerElWithoutChildren
            , containerEl (Fuzz.listOfLengthBetween 0 5 (elWithMaxDepth (depth - 1)))
            ]


textEl : Fuzzer El
textEl =
    Fuzz.map (\s -> Text [] s) Fuzz.string


emptyEl : Fuzzer El
emptyEl =
    Fuzz.constant Empty


containerElWithoutChildren : Fuzzer El
containerElWithoutChildren =
    containerEl (Fuzz.constant [])


containerEl : Fuzzer (List El) -> Fuzzer El
containerEl childrenFuzzer =
    Fuzz.map2 Container attrsFuzzer childrenFuzzer


attrsFuzzer : Fuzzer (List Attr)
attrsFuzzer =
    Fuzz.listOfLengthBetween 0 5 attrFuzzer


attrFuzzer : Fuzzer Attr
attrFuzzer =
    Fuzz.oneOf
        [ Fuzz.map LayoutDirection layoutDirectionFuzzer
        , Fuzz.map Width sizeFuzzer
        , Fuzz.map Height sizeFuzzer
        , Fuzz.map4 Padding Fuzz.int Fuzz.int Fuzz.int Fuzz.int
        , Fuzz.map ChildGap Fuzz.int
        , Fuzz.map BgColor colorFuzzer
        , Fuzz.map HorizAlign horizAlignFuzzer
        , Fuzz.map VertAlign vertAlignFuzzer
        ]


layoutDirectionFuzzer : Fuzzer LayoutDirection
layoutDirectionFuzzer =
    Fuzz.oneOfValues
        [ TopToBottom
        , LeftToRight
        ]


sizeFuzzer : Fuzzer Size
sizeFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Fixed Fuzz.int
        , Fuzz.map Fit (Fuzz.listOfLengthBetween 0 3 sizeAttrFuzzer)
        , Fuzz.map Grow (Fuzz.listOfLengthBetween 0 3 sizeAttrFuzzer)
        ]


sizeAttrFuzzer : Fuzzer SizeAttr
sizeAttrFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Min Fuzz.int
        , Fuzz.map Max Fuzz.int
        ]


colorFuzzer : Fuzzer Color
colorFuzzer =
    Fuzz.oneOfValues
        [ Purple
        , LightPurple
        , Blue
        , Pink
        , Yellow
        ]


horizAlignFuzzer : Fuzzer HorizAlign
horizAlignFuzzer =
    Fuzz.oneOfValues
        [ Left
        , HCenter
        , Right
        ]


vertAlignFuzzer : Fuzzer VertAlign
vertAlignFuzzer =
    Fuzz.oneOfValues
        [ Top
        , VCenter
        , Bottom
        ]
