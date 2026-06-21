module Step3WrapText exposing (charHeight, charWidth, wrapText)

{-| TODO: monospace fonts only for now, to simplify the text bbox computation aspect of things.
Later we can try to figure out how to read TTF fonts or simple proportional bitmap fonts.

TODO: some kind of line spacing - gap between lines? Fencepost formula

-}

import El exposing (..)
import Log


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
    El.mapPostOrderWithParent
        (\maybeParent ((AEl ael) as ael_) ->
            case ael.text of
                Nothing ->
                    -- not text, but we might have had text descendants. We need to do the Fit phase again
                    -- This mostly copies Step1 code.
                    --
                    -- TODO can we for example do the text sizing step first
                    -- (between step0 and step1) and then do the Fit sizing just
                    -- once? How does it interact with maxWidth?
                    let
                        { along } =
                            El.axes ael_
                    in
                    case along.getSizeSpec ael_ of
                        SFixed n ->
                            -- These are already done and don't need to change (step1)
                            ael_

                        SFit ->
                            ael_
                                |> along.setSize
                                    (List.sum (List.map along.getSize ael.children)
                                        + along.getPaddingStart ael_
                                        + along.getPaddingEnd ael_
                                        + {- TODO PERF count them once in Step 0 -} (max 0 (List.length ael.children - 1) * ael.childGap)
                                    )

                        SGrow ->
                            -- Do nothing in this step (Grow is handled elsewhere)
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
