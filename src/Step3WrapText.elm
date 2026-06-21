module Step3WrapText exposing (wrapText)

{-| Measure size of text and wrap the text according to max width constraints.
Also propagate the new widths to the ancestors.

TODO: monospace fonts only for now, to simplify the text bbox computation aspect of things.
Later we can try to figure out how to read TTF fonts or simple proportional bitmap fonts.

TODO: some kind of line spacing - gap between lines? Fencepost formula

-}

import El exposing (..)
import Log
import Text


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
                |> Maybe.map (\pWidth -> pWidth // Text.charWidth)

        lines =
            case maxCharsPerLine of
                Nothing ->
                    textLines

                Just maxChars ->
                    textLines
                        -- this depends on wrapAnywhere checking the "no width available" case for us
                        |> List.concatMap (greedyGroupsOf maxChars)

        { width, height } =
            Text.measure lines
    in
    { lines = lines
    , width = width
    , height = height
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
                Just text ->
                    let
                        { lines, width, height } =
                            -- TODO widthMax? widthMin?
                            wrapAnywhere
                                { maxWidth =
                                    maybeParent
                                        |> Maybe.andThen
                                            (\((AEl parent) as parent_) ->
                                                case parent.widthSpec of
                                                    SFit ->
                                                        -- ?
                                                        Nothing

                                                    SGrow ->
                                                        -- ?
                                                        Nothing

                                                    SFixed n ->
                                                        -- TODO maybe parent.width?
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

                Nothing ->
                    -- Propagate the new widths
                    -- This mostly copies step 1.
                    case ael.widthSpec of
                        SFixed n ->
                            -- Don't deal with Fixed here - we have done that in step 1.
                            ael_

                        SGrow ->
                            -- Don't deal with Grow here - we have done that in step 1.
                            ael_

                        SFit ->
                            -- TODO can we do things in a less "throw away the
                            -- original computation" way? Only propagate where
                            -- it makes sense?
                            AEl
                                { ael
                                    | width =
                                        let
                                            childWidths =
                                                ael.children
                                                    |> List.map (El.inner .width)
                                        in
                                        (case ael.layoutDirection of
                                            LeftToRight ->
                                                List.sum childWidths
                                                    + -- TODO PERF count them once in Step 0
                                                      max 0 ((List.length ael.children - 1) * ael.childGap)

                                            TopToBottom ->
                                                List.maximum childWidths
                                                    |> Maybe.withDefault 0
                                        )
                                            + ael.paddingLeft
                                            + ael.paddingRight
                                }
        )
        root
        |> Log.annotatedEl "step 3 - wrap text"
