module Example exposing (holyGrail, main)

import El exposing (..)
import Html exposing (Html)
import Layout
import Render


main : Html msg
main =
    let
        annotated =
            Layout.layout
                { layoutWidth = 640
                , layoutHeight = 480
                }
                holyGrail
    in
    Html.div []
        [ Html.div [] [ Render.htmlTranslate annotated ]
        , Html.div [] [ Render.debug annotated ]
        ]


holyGrail : El
holyGrail =
    Container
        [ LayoutDirection TopToBottom
        , Width (Grow [])
        , Height (Grow [])
        ]
        [ -- header
          Container
            [ LayoutDirection LeftToRight
            , Width (Grow [])
            , ChildGap 16
            , Padding 8 8 8 8
            , BgColor Purple
            ]
            [ -- title
              Container
                [ Width (Grow [])
                , HorizAlign Left
                ]
                [ Text [] "Title" ]
            , -- login
              Text [] "Login"
            ]
        , -- main
          Container
            [ LayoutDirection LeftToRight
            , Width (Grow [])
            , Height (Grow [])
            ]
            [ -- left rail
              Container
                [ LayoutDirection TopToBottom
                , Height (Grow [])
                , HorizAlign Left
                , Padding 8 8 8 8
                , ChildGap 16
                , BgColor LightPurple
                ]
                [ -- Top section of left rail
                  Container
                    [ Height (Grow [])
                    , ChildGap 8
                    ]
                    [ Text [] "Home"
                    , Text [] "About me"
                    , Text [] "Downloads"
                    ]
                , -- Bottom section of left rail
                  Container []
                    [ Text [] "Admin link"
                    ]
                ]
            , -- content
              Container
                [ LayoutDirection TopToBottom
                , Width (Grow [])
                , Height (Grow [])
                , BgColor Pink
                , ChildGap 8
                , Padding 8 8 8 8
                ]
                [ Text [] "Heading!"
                , Text [] "Some text"
                , Text [] "Some more text here."
                ]
            , -- right rail
              Container
                [ LayoutDirection TopToBottom
                , Height (Grow [])
                , BgColor LightPurple
                , ChildGap 8
                , Padding 8 8 8 8
                ]
                [ Text [] "On this page"
                , Text [] "Tags"
                , Text [] "Reading time"
                ]
            ]
        , -- footer
          Container
            [ Width (Grow [])
            , BgColor Purple
            , Padding 8 8 8 8
            , HorizAlign HCenter
            , VertAlign VCenter
            ]
            [ Text [] "(c) 2026 MJ" ]
        ]
