module Step0Annotate exposing (annotate)

import El exposing (..)


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
                    { el | heightSpec = SFixed (max 0 n) }

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
                | paddingTop = max 0 t
                , paddingRight = max 0 r
                , paddingBottom = max 0 b
                , paddingLeft = max 0 l
            }

        ChildGap gap ->
            { el | childGap = max 0 gap }

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
            { el | widthMin = Just (max 0 n) }

        Max n ->
            { el | widthMax = Just (max 0 n) }


applyHeightSizeAttr : SizeAttr -> AnnotatedElData -> AnnotatedElData
applyHeightSizeAttr attr el =
    case attr of
        Min n ->
            { el | heightMin = Just (max 0 n) }

        Max n ->
            { el | heightMax = Just (max 0 n) }
