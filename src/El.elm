module El exposing
    ( El(..)
    , AnnotatedEl(..), AnnotatedElData, empty
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
@docs AnnotatedEl, AnnotatedElData, empty

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
    { x : Float
    , y : Float
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
    , setSize = \w (AEl ael2) -> AEl { ael2 | width = w }
    , paddingStart = ael.paddingLeft
    , paddingEnd = ael.paddingRight
    }


vertAxis : AnnotatedElData -> Axis
vertAxis ael =
    { sizeSpec = ael.heightSpec
    , getSize = \(AEl ael2) -> ael2.height
    , setSize = \h (AEl ael2) -> AEl { ael2 | height = h }
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
