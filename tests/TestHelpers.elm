module TestHelpers exposing (el, elWithMaxDepth, bgColorFromEl)

import El exposing (..)
import Fuzz exposing (Fuzzer)


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
            , containerEl (Fuzz.list (elWithMaxDepth (depth - 1)))
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
    Fuzz.list attrFuzzer


attrFuzzer : Fuzzer Attr
attrFuzzer =
    Fuzz.oneOf
        [ Fuzz.map LayoutDirection layoutDirectionFuzzer
        , Fuzz.map Width sizeFuzzer
        , Fuzz.map Height sizeFuzzer
        , Fuzz.map4 Padding Fuzz.int Fuzz.int Fuzz.int Fuzz.int
        , Fuzz.map ChildGap Fuzz.int
        , Fuzz.map BgColor colorFuzzer
        , Fuzz.map ImageData Fuzz.string
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
        , Fuzz.map Fit (Fuzz.list sizeAttrFuzzer)
        , Fuzz.map Grow (Fuzz.list sizeAttrFuzzer)
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


bgColorFromEl : El -> Maybe Color
bgColorFromEl input =
    case input of
        Container attrs _ ->
            findBgColor attrs

        Text _ _ ->
            Nothing

        Empty ->
            Nothing


findBgColor : List Attr -> Maybe Color
findBgColor attrs =
    case attrs of
        [] ->
            Nothing

        BgColor color :: _ ->
            Just color

        _ :: rest ->
            findBgColor rest
