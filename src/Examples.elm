module Main exposing (ex1, ex2, ex3)

import El exposing (..)


ex1 : El
ex1 =
    Container
        [ LayoutDirection TopToBottom
        , BgColor Purple
        ]
        []


ex2 : El
ex2 =
    let
        viewMenuItem : ( String, String ) -> El
        viewMenuItem ( label, icon ) =
            Container
                [ Width (Grow [])
                , BgColor LightPurple
                ]
                [ Text [ FontSize 18 ] label
                , Container [] [ Text [] icon ]
                ]
    in
    Container
        [ LayoutDirection TopToBottom
        , BgColor Purple
        ]
        ([ ( "Copy", "copy" )
         , ( "Paste", "paste" )
         ]
            |> List.map viewMenuItem
        )


ex3 : El
ex3 =
    Container
        [ Width (Fixed 960)
        , Height (Fixed 540)
        , BgColor Blue
        , Padding 32 32 32 32
        , ChildGap 32
        ]
        [ Container
            [ Width (Fixed 300)
            , Height (Fixed 300)
            , BgColor Pink
            ]
            []
        , Container
            [ Width (Fixed 350)
            , Height (Fixed 200)
            , BgColor Yellow
            ]
            []
        ]
