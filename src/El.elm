module El exposing
    ( El(..)
    , AnnotatedEl(..), AnnotatedElData, empty, printout
    , Attr(..)
    , LayoutDirection(..), axes
    , Size(..), SizeAttr(..), SizeSpec(..)
    , HorizAlign(..), VertAlign(..)
    , Color(..)
    , TextAttr(..)
    , preOrder, postOrder
    , mapPreOrder, mapPostOrder
    , foldPreOrder, foldPostOrder
    , mapAccumPreOrder, mapAccumPostOrder
    )

{-|

@docs El
@docs AnnotatedEl, AnnotatedElData, empty, AnnotatedElPrintout, printout

@docs Attr
@docs LayoutDirection, axes
@docs Size, SizeAttr, SizeSpec
@docs HorizAlign, VertAlign
@docs Color
@docs TextAttr

@docs preOrder, postOrder
@docs mapPreOrder, mapPostOrder
@docs foldPreOrder, foldPostOrder
@docs mapAccumPreOrder, mapAccumPostOrder

-}


type El
    = Container (List Attr) (List El)
    | Text (List TextAttr) String
    | Empty


type AnnotatedEl
    = AEl AnnotatedElData


type alias AnnotatedElData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , layoutDirection : LayoutDirection
    , horizAlign : HorizAlign
    , vertAlign : VertAlign
    , children : List AnnotatedEl
    , bgColor : Maybe Color
    , fontSize : Maybe Int
    , childGap : Int
    , text : Maybe String
    , widthSpec : SizeSpec
    , widthMin : Maybe Int
    , widthMax : Maybe Int
    , heightSpec : SizeSpec
    , heightMin : Maybe Int
    , heightMax : Maybe Int
    , paddingTop : Int
    , paddingRight : Int
    , paddingBottom : Int
    , paddingLeft : Int
    }


type Attr
    = LayoutDirection LayoutDirection
    | Width Size
    | Height Size
    | Padding Int Int Int Int
    | ChildGap Int
    | BgColor Color
    | HorizAlign HorizAlign
    | VertAlign VertAlign


type HorizAlign
    = Left
    | HCenter
    | Right


type VertAlign
    = Top
    | VCenter
    | Bottom


type LayoutDirection
    = TopToBottom
    | {- DEFAULT -} LeftToRight


type Size
    = Fixed Int
    | {- DEFAULT -} Fit (List SizeAttr)
    | Grow (List SizeAttr)


type SizeSpec
    = SFixed Int
    | SFit
    | SGrow


type SizeAttr
    = Min Int
    | Max Int


type Color
    = Purple
    | LightPurple
    | Blue
    | Pink
    | Yellow


type TextAttr
    = FontSize Int


foldPreOrder : (AnnotatedEl -> acc -> acc) -> acc -> AnnotatedEl -> acc
foldPreOrder step acc ((AEl ael) as ael_) =
    List.foldl
        (\child childAcc -> foldPreOrder step childAcc child)
        (step ael_ acc)
        ael.children


foldPostOrder : (AnnotatedEl -> acc -> acc) -> acc -> AnnotatedEl -> acc
foldPostOrder step acc ((AEl ael) as ael_) =
    step ael_
        (List.foldl
            (\child childAcc -> foldPostOrder step childAcc child)
            acc
            ael.children
        )


preOrder : AnnotatedEl -> List AnnotatedEl
preOrder el =
    foldPreOrder (::) [] el


postOrder : AnnotatedEl -> List AnnotatedEl
postOrder el =
    foldPostOrder (::) [] el


mapPreOrder : (AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPreOrder fn root =
    let
        (AEl root_) =
            fn root
    in
    AEl { root_ | children = List.map fn root_.children }


mapPostOrder : (AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPostOrder fn (AEl root) =
    let
        root_ =
            { root | children = List.map fn root.children }
    in
    fn (AEl root_)


mapAccumPreOrder : (AnnotatedEl -> acc -> ( acc, AnnotatedEl )) -> acc -> AnnotatedEl -> ( acc, AnnotatedEl )
mapAccumPreOrder fn acc ((AEl root) as root_) =
    let
        ( acc_, AEl root__ ) =
            fn root_ acc

        ( acc__, children ) =
            List.foldl
                (\child ( childAcc, doneChildren ) ->
                    let
                        ( childAcc_, child_ ) =
                            mapAccumPreOrder fn childAcc child
                    in
                    ( childAcc_, child_ :: doneChildren )
                )
                ( acc, [] )
                root.children
    in
    ( acc__, AEl { root__ | children = children } )


mapAccumPostOrder : (AnnotatedEl -> acc -> ( acc, AnnotatedEl )) -> acc -> AnnotatedEl -> ( acc, AnnotatedEl )
mapAccumPostOrder fn acc ((AEl root) as root_) =
    let
        ( acc_, children ) =
            List.foldl
                (\child ( childAcc, doneChildren ) ->
                    let
                        ( childAcc_, child_ ) =
                            mapAccumPostOrder fn childAcc child
                    in
                    ( childAcc_, child_ :: doneChildren )
                )
                ( acc, [] )
                root.children

        ( acc__, AEl root__ ) =
            fn root_ acc_
    in
    ( acc__, AEl { root__ | children = children } )



-- Axis


type alias Axis =
    { sizeSpec : SizeSpec
    , getSize : AnnotatedEl -> Int
    , setSize : Int -> AnnotatedEl -> AnnotatedEl
    , paddingStart : Int
    , paddingEnd : Int
    }


horizAxis : AnnotatedElData -> Axis
horizAxis ael =
    { sizeSpec = ael.widthSpec
    , getSize = \(AEl ael2) -> ael2.width
    , setSize = \w (AEl ael2) -> AEl { ael2 | width = max 0 w }
    , paddingStart = ael.paddingLeft
    , paddingEnd = ael.paddingRight
    }


vertAxis : AnnotatedElData -> Axis
vertAxis ael =
    { sizeSpec = ael.heightSpec
    , getSize = \(AEl ael2) -> ael2.height
    , setSize = \h (AEl ael2) -> AEl { ael2 | height = max 0 h }
    , paddingStart = ael.paddingTop
    , paddingEnd = ael.paddingBottom
    }


axes : AnnotatedEl -> { along : Axis, across : Axis }
axes (AEl ael) =
    case ael.layoutDirection of
        LeftToRight ->
            { along = horizAxis ael
            , across = vertAxis ael
            }

        TopToBottom ->
            { along = vertAxis ael
            , across = horizAxis ael
            }



----------


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


printout : AnnotatedEl -> String
printout (AEl ael) =
    let
        positionAndSize : String
        positionAndSize =
            String.fromInt ael.width
                ++ "x"
                ++ String.fromInt ael.height
                ++ " @ "
                ++ String.fromInt ael.x
                ++ ","
                ++ String.fromInt ael.y

        diff : String -> (AnnotatedElData -> a) -> (a -> String) -> Maybe String
        diff label get format =
            let
                got : a
                got =
                    get ael
            in
            if got == get empty then
                Nothing

            else
                Just (label ++ ": " ++ format got)

        sizeSpecToString : ( SizeSpec, Maybe Int, Maybe Int ) -> String
        sizeSpecToString ( ss, min, max ) =
            let
                common label =
                    label
                        ++ (case min of
                                Nothing ->
                                    ""

                                Just min_ ->
                                    " (min: " ++ String.fromInt min_ ++ ")"
                           )
                        ++ (case max of
                                Nothing ->
                                    ""

                                Just max_ ->
                                    " (max: " ++ String.fromInt max_ ++ ")"
                           )
            in
            case ss of
                SFixed n ->
                    "Fixed " ++ String.fromInt n

                SFit ->
                    common "Fit"

                SGrow ->
                    common "Grow"

        maybe : (a -> String) -> Maybe a -> String
        maybe fn mb =
            case mb of
                Nothing ->
                    "Nothing"

                Just a ->
                    fn a
    in
    [ diff "x" .x String.fromInt
    , diff "y" .y String.fromInt
    , diff "width" .width String.fromInt
    , diff "height" .height String.fromInt
    , diff "children" .children (\ch -> String.fromInt (List.length ch) ++ "x")
    , diff "layoutDirection"
        .layoutDirection
        (\ld ->
            case ld of
                LeftToRight ->
                    "LeftToRight"

                TopToBottom ->
                    "TopToBottom"
        )
    , diff "horizAlign"
        .horizAlign
        (\ha ->
            case ha of
                Left ->
                    "Left"

                HCenter ->
                    "HCenter"

                Right ->
                    "Right"
        )
    , diff "vertAlign"
        .vertAlign
        (\va ->
            case va of
                Top ->
                    "Top"

                VCenter ->
                    "VCenter"

                Bottom ->
                    "Bottom"
        )
    , diff "widthSpec" (\ael_ -> ( ael_.widthSpec, ael_.widthMin, ael_.widthMax )) sizeSpecToString
    , diff "heightSpec" (\ael_ -> ( ael_.heightSpec, ael_.heightMin, ael_.heightMax )) sizeSpecToString
    , diff "paddingTop" .paddingTop String.fromInt
    , diff "paddingRight" .paddingRight String.fromInt
    , diff "paddingBottom" .paddingBottom String.fromInt
    , diff "paddingLeft" .paddingLeft String.fromInt
    , diff "childGap" .childGap String.fromInt
    , diff "bgColor"
        .bgColor
        (maybe
            (\c ->
                case c of
                    Blue ->
                        "Blue"

                    Yellow ->
                        "Yellow"

                    Purple ->
                        "Purple"

                    LightPurple ->
                        "LightPurple"

                    Pink ->
                        "Pink"
            )
        )
    , diff "fontSize" .fontSize (maybe String.fromInt)
    , diff "text" .text (maybe (\s -> "\"" ++ s ++ "\""))
    ]
        |> List.filterMap identity
        |> List.map (\s -> "  " ++ s)
        |> String.join "\n"
        |> (\s -> positionAndSize ++ " [\n" ++ s ++ "\n]")
