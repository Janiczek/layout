module Step0Annotate exposing (annotate)

import El exposing (..)


empty : AnnotatedElData
empty =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , children = []
    , layoutDirection = LeftToRight
    , horizAlign = HCenter
    , vertAlign = VCenter
    , widthSpec = SFit
    , widthMin = Nothing
    , widthMax = Nothing
    , heightSpec = SFit
    , heightMin = Nothing
    , heightMax = Nothing
    , paddingTop = 0
    , paddingRight = 0
    , paddingBottom = 0
    , paddingLeft = 0
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
            case size of
                Fixed n ->
                    { el | widthSpec = SFixed n }

                Fit attrs ->
                    List.foldl
                        applyWidthSizeAttr
                        { el | widthSpec = SFit }
                        attrs

                Grow attrs ->
                    List.foldl
                        applyWidthSizeAttr
                        { el | widthSpec = SGrow }
                        attrs

        Height size ->
            case size of
                Fixed n ->
                    { el | heightSpec = SFixed n }

                Fit attrs ->
                    List.foldl
                        applyHeightSizeAttr
                        { el | heightSpec = SFit }
                        attrs

                Grow attrs ->
                    List.foldl
                        applyHeightSizeAttr
                        { el | heightSpec = SGrow }
                        attrs

        Padding t r b l ->
            { el
                | paddingTop = t
                , paddingRight = r
                , paddingBottom = b
                , paddingLeft = l
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


applyWidthSizeAttr : SizeAttr -> AnnotatedElData -> AnnotatedElData
applyWidthSizeAttr attr el =
    case attr of
        Min n ->
            { el | widthMin = Just n }

        Max n ->
            { el | widthMax = Just n }


applyHeightSizeAttr : SizeAttr -> AnnotatedElData -> AnnotatedElData
applyHeightSizeAttr attr el =
    case attr of
        Min n ->
            { el | heightMin = Just n }

        Max n ->
            { el | heightMax = Just n }
