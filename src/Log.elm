module Log exposing (annotatedEl)

import El exposing (AnnotatedEl)


annotatedEl : String -> AnnotatedEl -> AnnotatedEl
annotatedEl label ael =
    let
        _ =
            Debug.log (El.printout ael) label
    in
    ael
