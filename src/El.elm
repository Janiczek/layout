module El exposing
    ( El(..)
    , AnnotatedEl(..), AnnotatedElData, empty, printout, inner
    , Config
    , Attr(..)
    , LayoutDirection(..)
    , Size(..), SizeAttr(..), SizeSpec(..)
    , HorizAlign(..), VertAlign(..)
    , Color(..), colorToHtmlColor
    , TextAttr(..)
    , preOrder, postOrder
    , mapPreOrder, mapPostOrder
    , mapPreOrderWithParent, mapPostOrderWithParent
    )

{-|

@docs El
@docs AnnotatedEl, AnnotatedElData, empty, AnnotatedElPrintout, printout, inner
@docs Config

@docs Attr
@docs LayoutDirection
@docs Size, SizeAttr, SizeSpec
@docs HorizAlign, VertAlign
@docs Color, colorToHtmlColor
@docs TextAttr

@docs preOrder, postOrder
@docs mapPreOrder, mapPostOrder
@docs mapPreOrderWithParent, mapPostOrderWithParent

---

Width of grow() = technically 0 but could represent it as "Hasn't grown yet"

Grow child elements:

  - remaining width = parent width - left+right paddings - child widths - all gaps
  - give the remaining width to all GROW children (not set! redistribute to them, +=)
  - remaining height = parent height - top+bottom paddings
  - give the remaining height to the GROW children

Grow = while there is some remaining width: (<https://youtu.be/by9lQvpvMIc?t=1590>)

  - expand the smallest elements until they reach the size of the next smallest elements
    Shrink = dual
  - when we find a shrinkable element (one of the currently largest) is already
    at its min size, remove it from the list

Text preferential width = if it had unlimited space available
Text minimum width = width of its largest word?

Shrinking: dual of growing
shrink largest elements (that aren't their minimum widths)
at the same rate until you hit the next largest thing

Need a way to measure text bbox

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
    , childGap : Int
    , text : Maybe String
    , fontColor : Maybe Color
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
    = Black
    | Purple
    | LightPurple
    | Blue
    | Pink
    | Yellow


colorToHtmlColor : Color -> String
colorToHtmlColor c =
    case c of
        Black ->
            "black"

        Blue ->
            "blue"

        Yellow ->
            "yellow"

        Purple ->
            "purple"

        LightPurple ->
            "plum"

        Pink ->
            "pink"


type TextAttr
    = FontColor Color


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


{-| Run function on every AnnotatedEl in this tree.
Depth-first pre-order: root is done before the children are done.
-}
mapPreOrder : (AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPreOrder fn root =
    let
        (AEl root_) =
            fn root
    in
    AEl { root_ | children = List.map (mapPreOrder fn) root_.children }


{-| Run function on every AnnotatedEl in this tree.
Depth-first post-order: children are done before the root is done.
-}
mapPostOrder : (AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPostOrder fn (AEl root) =
    let
        root_ =
            { root | children = List.map (mapPostOrder fn) root.children }
    in
    fn (AEl root_)


{-| Run function on every AnnotatedEl in this tree.
Depth-first pre-order: root is done before the children are done.
Gives the function the parent AnnotatedEl as well (Nothing = root)
-}
mapPreOrderWithParent : (Maybe AnnotatedEl -> AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPreOrderWithParent fn root =
    let
        aux : Maybe AnnotatedEl -> AnnotatedEl -> AnnotatedEl
        aux parent el =
            let
                ((AEl newEl_) as newEl) =
                    fn parent el

                children =
                    newEl_.children
                        |> List.map (aux (Just newEl))
            in
            AEl { newEl_ | children = children }
    in
    aux Nothing root


{-| Run function on every AnnotatedEl in this tree.
Depth-first post-order: root is done before the children are done.
Gives the function the parent AnnotatedEl as well (Nothing = root)
-}
mapPostOrderWithParent : (Maybe AnnotatedEl -> AnnotatedEl -> AnnotatedEl) -> AnnotatedEl -> AnnotatedEl
mapPostOrderWithParent fn root =
    let
        aux : Maybe AnnotatedEl -> AnnotatedEl -> AnnotatedEl
        aux parent ((AEl el) as el_) =
            let
                newEl =
                    { el | children = List.map (aux (Just el_)) el.children }
            in
            fn parent (AEl newEl)
    in
    aux Nothing root


type alias Config =
    { layoutWidth : Int
    , layoutHeight : Int
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
    , fontColor = Nothing
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
    [ {- diff "x" .x String.fromInt
         , diff "y" .y String.fromInt
         , diff "width" .width String.fromInt
         , diff "height" .height String.fromInt
         ,
      -}
      diff "layoutDirection"
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
    , diff "bgColor" .bgColor (maybe colorToHtmlColor)
    , diff "fontColor" .fontColor (maybe colorToHtmlColor)
    , diff "text" .text (maybe printoutText)
    , diff "children"
        .children
        (\ch ->
            "\n"
                ++ (ch
                        |> List.map (\child -> printout child |> indent 4)
                        |> String.join ",\n"
                   )
        )
    ]
        |> List.filterMap identity
        |> List.map (\s -> "  " ++ s)
        |> String.join "\n"
        |> (\s -> positionAndSize ++ " [\n" ++ s ++ "\n]")


printoutText : String -> String
printoutText s =
    ("\n\"\"\"\n" ++ s ++ "\n\"\"\"")
        |> indent 4


indent : Int -> String -> String
indent n str =
    str
        |> String.lines
        |> List.map (\s -> String.repeat n " " ++ s)
        |> String.join "\n"


inner : (AnnotatedElData -> a) -> AnnotatedEl -> a
inner fn (AEl ael) =
    fn ael
