module Log exposing (annotatedEl)

import El exposing (AnnotatedEl)


shouldLog : Bool
shouldLog =
    False


log : String -> a -> a
log label thing =
    if shouldLog then
        Debug.log label thing

    else
        thing


annotatedEl : String -> AnnotatedEl -> AnnotatedEl
annotatedEl label ael =
    let
        _ =
            log (El.printout ael) label
    in
    ael
