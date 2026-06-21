module Text exposing (charHeight, charWidth, measure)


charWidth : Int
charWidth =
    6


charHeight : Int
charHeight =
    8


measure : List String -> { width : Int, height : Int }
measure lines =
    { width =
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
