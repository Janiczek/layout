module El exposing
    ( El(..), AnnotatedEl(..)
    , Attr(..)
    , LayoutDirection(..)
    , Size(..), SizeAttr(..)
    , HorizAlign(..), VertAlign(..)
    , Color(..)
    , TextAttr(..)
    , foldPreOrder, foldPostOrder
    , annotatedPreOrder, annotatedPostOrder
    )

{-|

@docs El, AnnotatedEl

@docs Attr
@docs LayoutDirection
@docs Size, SizeAttr
@docs HorizAlign, VertAlign
@docs Color
@docs TextAttr

@docs foldPreOrder, foldPostOrder
@docs annotatedPreOrder, annotatedPostOrder

-}


type El
    = Container (List Attr) (List El)
    | Text (List TextAttr) String
    | Empty


type AnnotatedEl
    = AEl
        { position :
            { x : Float
            , y : Float
            }
        , size :
            { width : Float
            , height : Float
            }
        , children : List AnnotatedEl
        , bgColor : Maybe Color

        -- TODO: extra data like text string / image etc.
        }


type Attr
    = LayoutDirection LayoutDirection
    | Width Size
    | Height Size
    | Padding Int Int Int Int
    | ChildGap Int
    | BgColor Color
    | ImageData String
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


foldPreOrder : (El -> acc -> acc) -> acc -> El -> acc
foldPreOrder step acc el =
    case el of
        Container _ children ->
            List.foldl
                (\child childAcc -> foldPreOrder step childAcc child)
                (step el acc)
                children

        Text _ _ ->
            step el acc

        Empty ->
            step el acc


foldPostOrder : (El -> acc -> acc) -> acc -> El -> acc
foldPostOrder step acc el =
    case el of
        Container _ children ->
            step el 
                (List.foldl
                    (\child childAcc -> foldPostOrder step childAcc child)
                    acc
                    children
                )

        Text _ _ ->
            step el acc

        Empty ->
            step el acc


annotatedPreOrder : AnnotatedEl -> List AnnotatedEl
annotatedPreOrder annotated =
    case annotated of
        AEl { children } ->
            annotated :: List.concatMap annotatedPreOrder children


annotatedPostOrder : AnnotatedEl -> List AnnotatedEl
annotatedPostOrder annotated =
    case annotated of
        AEl { children } ->
            List.concatMap annotatedPostOrder children ++ [ annotated ]
