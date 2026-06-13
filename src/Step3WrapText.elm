module Step3WrapText exposing (charHeight, charWidth, wrapText)

{-| TODO: monospace fonts only for now, to simplify the text bbox computation aspect of things.
Later we can try to figure out how to read TTF fonts or simple proportional bitmap fonts.

TODO: some kind of line spacing - gap between lines? Fencepost formula

-}

import El exposing (..)


charWidth : Int
charWidth =
    6


charHeight : Int
charHeight =
    8


{-| Wraps at any character instead of spaces, hyphens etc.
-}
wrapAnywhere :
    { maxWidth : Maybe Int
    , text : String
    }
    ->
        { lines : List String
        , width : Int
        , height : Int
        }
wrapAnywhere r =
    case r.maxWidth of
        Just n ->
            if n <= 0 then
                { lines = []
                , width = 0
                , height = 0
                }

            else
                wrapAnywhereUnchecked r

        Nothing ->
            wrapAnywhereUnchecked r


wrapAnywhereUnchecked :
    { maxWidth : Maybe Int
    , text : String
    }
    ->
        { lines : List String
        , width : Int
        , height : Int
        }
wrapAnywhereUnchecked { maxWidth, text } =
    let
        textLines : List String
        textLines =
            String.lines text

        maxCharsPerLine : Maybe Int
        maxCharsPerLine =
            -- eg. 250px fits 41 6px chars
            maxWidth
                |> Maybe.map (\pWidth -> pWidth // charWidth)

        lines =
            case maxCharsPerLine of
                Nothing ->
                    textLines

                Just maxChars ->
                    textLines
                        -- this depends on wrapAnywhere checking the "no width available" case for us
                        |> List.concatMap (greedyGroupsOf maxChars)
    in
    { lines = lines
    , width =
        -- TODO horiz spacing between chars?
        lines
            |> List.map String.length
            |> List.maximum
            |> Maybe.withDefault 0
            |> (*) charWidth
    , height =
        -- TODO figure out line height later
        lines
            |> List.length
            |> (*) charHeight
    }


greedyGroupsOf : Int -> String -> List String
greedyGroupsOf maxLength str =
    if maxLength <= 0 then
        let
            _ =
                Debug.log "0 characters width available for text wrapping??" ()
        in
        []

    else
        let
            go : String -> List String -> List String
            go str_ revAcc =
                if String.isEmpty str_ then
                    List.reverse revAcc

                else
                    go
                        (String.dropLeft maxLength str_)
                        (String.left maxLength str_ :: revAcc)
        in
        go str []


wrapText : AnnotatedEl -> AnnotatedEl
wrapText root =
    El.mapPreOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            case ael.text of
                Nothing ->
                    -- not text
                    ael_

                Just text ->
                    let
                        { along } =
                            El.axes ael_

                        { lines, width, height } =
                            -- TODO widthMax? widthMin?
                            wrapAnywhere
                                { maxWidth =
                                    maybeParent
                                        |> Maybe.andThen
                                            (\((AEl parent) as parent_) ->
                                                case along.getSizeSpec parent_ of
                                                    SFit ->
                                                        -- ?
                                                        Nothing

                                                    SGrow ->
                                                        -- ?
                                                        Nothing

                                                    SFixed n ->
                                                        -- TODO maybe along.getSize? (width/height)
                                                        Just n
                                            )
                                , text = text
                                }
                    in
                    AEl
                        { ael
                            | width = width
                            , height = height
                            , text = Just (lines |> String.join "\n")
                        }
        )
        root
