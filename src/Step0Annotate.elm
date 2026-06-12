module Step0Annotate exposing (annotate)

import El exposing (..)


empty : AnnotatedElData
empty =
    { position = { x = 0, y = 0 }
    , size = { width = 0, height = 0 }
    , children = []
    , layoutDirection = LeftToRight
    , horizAlign = HCenter
    , vertAlign = VCenter
    , widthSpec = Fit []
    , heightSpec = Fit []
    , padding =
        { top = 0
        , right = 0
        , bottom = 0
        , left = 0
        }
    , childGap = 0
    , bgColor = Nothing
    , fontSize = Nothing
    , text = Nothing
    }


annotate : El -> AnnotatedEl
annotate el =
    AEl <|
        case el of
            Container attrs children ->
                List.foldl
                    applyAttr
                    { empty | children = List.map annotate children }
                    attrs

            Text attrs text ->
                List.foldl
                    applyTextAttr
                    { empty | text = Just text }
                    attrs

            Empty ->
                empty


applyAttr : Attr -> AnnotatedElData -> AnnotatedElData
applyAttr attr el =
    case attr of
        LayoutDirection dir ->
            { el | layoutDirection = dir }

        Width size ->
            { el | widthSpec = size }

        Height size ->
            { el | heightSpec = size }

        Padding t r b l ->
            { el
                | padding =
                    { top = t
                    , right = r
                    , bottom = b
                    , left = l
                    }
            }

        ChildGap gap ->
            { el | childGap = gap }

        BgColor color ->
            { el | bgColor = Just color }

        HorizAlign align ->
            { el | horizAlign = align }

        VertAlign align ->
            { el | vertAlign = align }


applyTextAttr : TextAttr -> AnnotatedElData -> AnnotatedElData
applyTextAttr attr el =
    case attr of
        FontSize size ->
            { el | fontSize = Just size }
