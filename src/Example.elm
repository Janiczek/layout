module Example exposing (holyGrail, main)

import El exposing (..)
import Html exposing (Html)
import Layout
import Render


main : Html msg
main =
    Render.htmlTranslate
        (Layout.layout
            { layoutWidth = 640
            , layoutHeight = 480
            }
            holyGrail
        )


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
                [ Width (Grow []) ]
                [ Text [] "Title" ]
            , -- login
              Container [] [ Text [] "Login" ]
            ]
        , -- main
          Container
            [ LayoutDirection LeftToRight
            , Width (Grow [])
            , Height (Grow [])
            ]
            [ -- TODO write these examples (some text) in all 3 center areas
              -- left rail
              Container
                [ LayoutDirection TopToBottom
                , Height (Grow [])
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
            ]
            [ Text [] "(c) 2026 MJ" ]
        ]
