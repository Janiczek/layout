module Text exposing (charHeight, charWidth, measure, viewHtml)

import Dict exposing (Dict)
import El exposing (Color)
import Html exposing (Html)
import Html.Attributes


charWidth : Int
charWidth =
    8


charHeight : Int
charHeight =
    10


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


charPixels : Char -> Maybe (List ( Int, Int ))
charPixels c =
    Dict.get c map


pixels : String -> List ( Int, Int )
pixels text =
    text
        |> String.lines
        |> List.indexedMap
            (\row line ->
                line
                    |> String.toList
                    |> List.indexedMap
                        (\col char ->
                            charPixels char
                                |> Maybe.map (addX (col * charWidth))
                        )
                    |> List.filterMap identity
                    |> List.concat
                    |> addY (row * charHeight)
            )
        |> List.concat


viewHtml : Color -> Int -> Int -> String -> Html msg
viewHtml color width height text =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "overflow" "visible"
        ]
        (text
            |> pixels
            |> List.map (pixelDot color)
        )


pixelDot : Color -> ( Int, Int ) -> Html msg
pixelDot color ( x, y ) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" "1px"
        , Html.Attributes.style "height" "1px"
        , Html.Attributes.style "background-color" (El.colorToHtmlColor color)
        , Html.Attributes.style "left" (String.fromInt x ++ "px")
        , Html.Attributes.style "top" (String.fromInt y ++ "px")
        ]
        []


addY : Int -> List ( Int, Int ) -> List ( Int, Int )
addY dy coords =
    List.map (\( x, y ) -> ( x, y + dy )) coords


addX : Int -> List ( Int, Int ) -> List ( Int, Int )
addX dx coords =
    List.map (\( x, y ) -> ( x + dx, y )) coords



-- Taken from VT220: http://github.com/janiczek/vt220-font-emulation


fromPbm : String -> List ( Int, Int )
fromPbm pbm =
    pbm
        |> String.trim
        |> String.lines
        |> List.indexedMap
            (\y line ->
                line
                    |> String.trim
                    |> String.toList
                    |> List.indexedMap
                        (\x c ->
                            ( ( x, y )
                            , case c of
                                '1' ->
                                    True

                                '0' ->
                                    False

                                _ ->
                                    Debug.todo "Invalid character"
                            )
                        )
            )
        |> List.concat
        |> List.filterMap
            (\( xy, isOn ) ->
                if isOn then
                    Just xy

                else
                    Nothing
            )


map : Dict Char (List ( Int, Int ))
map =
    [ ( 'A'
      , """
00000000
00010000
00101000
01000100
10000010
11111110
10000010
10000010
00000000
00000000
        """
      )
    , ( 'B'
      , """
00000000
11111100
01000010
01000010
01111100
01000010
01000010
11111100
00000000
00000000
        """
      )
    , ( 'C'
      , """
00000000
00111100
01000010
10000000
10000000
10000000
01000010
00111100
00000000
00000000
        """
      )
    , ( 'D'
      , """
00000000
11111000
01000100
01000010
01000010
01000010
01000100
11111000
00000000
00000000
        """
      )
    , ( 'b'
      , """
00000000
10000000
10000000
10111100
11000010
10000010
11000010
10111100
00000000
00000000
        """
      )
    , ( 'c'
      , """
00000000
00000000
00000000
01111100
10000010
10000000
10000000
01111110
00000000
00000000
        """
      )
    , ( 'd'
      , """
00000000
00000010
00000010
01111010
10000110
10000010
10000110
01111010
00000000
00000000
        """
      )

    --
    , ( '!'
      , """
00000000
00010000
00010000
00010000
00010000
00010000
00000000
00010000
00000000
00000000
        """
      )
    , ( '0'
      , """
00000000
00111000
01000100
10000010
10000010
10000010
01000100
00111000
00000000
00000000
        """
      )
    , ( '1'
      , """
00000000
00010000
00110000
01010000
00010000
00010000
00010000
01111100
00000000
00000000
        """
      )
    , ( '2'
      , """
00000000
01111100
10000010
00000010
00011100
01100000
10000000
11111110
00000000
00000000
        """
      )
    , ( '3'
      , """
00000000
11111110
00000100
00001000
00011100
00000010
10000010
01111100
00000000
00000000
        """
      )
    , ( ' '
      , """
00000000
00000000
00000000
00000000
00000000
00000000
00000000
00000000
00000000
00000000
        """
      )
    ]
        |> List.map (\( c, pbm ) -> ( c, fromPbm pbm ))
        |> Dict.fromList
