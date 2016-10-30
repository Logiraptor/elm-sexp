module Sexp.Decode exposing (..)

import Sexp.Encode
import Sexp.Lexer exposing (Token(..))
import String


type Decoder a
    = Decoder (Context -> Result String ( a, List Token ))


type alias Value =
    Sexp.Encode.Value


type alias Context =
    List Token


decodeString : Decoder a -> String -> Result String a
decodeString (Decoder d) s =
    (Sexp.Lexer.lex s
        `Result.andThen` d
    )
        |> Result.map fst


string : Decoder String
string =
    let
        decoder l =
            case l of
                (String s) :: rest ->
                    Ok ( s, rest )

                _ ->
                    Err "Expected String"
    in
        Decoder decoder


int : Decoder Int
int =
    let
        decoder l =
            case l of
                (Int i) :: rest ->
                    Ok ( i, rest )

                _ ->
                    Err "Expected Int"
    in
        Decoder decoder


float : Decoder Float
float =
    let
        decoder l =
            case l of
                (Float f) :: rest ->
                    Ok ( f, rest )

                _ ->
                    Err "Expected Float"
    in
        Decoder decoder


nil : Decoder ()
nil =
    let
        decoder l =
            case l of
                LParen :: RParen :: rest ->
                    Ok ( (), rest )

                _ ->
                    Err "Expected ()"
    in
        Decoder decoder


list : Decoder a -> Decoder (List a)
list (Decoder inner) =
    let
        nextOrEnd ( sofar, rest ) =
            case rest of
                RParen :: next ->
                    Ok ( List.reverse sofar, next )

                _ :: _ ->
                    Result.map
                        (\( newElem, afterThat ) ->
                            ( newElem :: sofar, afterThat )
                        )
                        (inner rest)
                        `Result.andThen` nextOrEnd

                [] ->
                    Err
                        ("Unexpected EOF looking for " ++ toString RParen)

        decoder l =
            (Result.map (\afterLParen -> ( [], afterLParen )) (require LParen l))
                `Result.andThen` nextOrEnd
    in
        Decoder decoder


require : Token -> List Token -> Result String (List Token)
require token list =
    case list of
        head :: tail ->
            if head == token then
                Ok tail
            else
                Err ("Expected " ++ (toString token) ++ ", found " ++ (toString head))

        [] ->
            Err ("Unexpected EOF looking for " ++ (toString token))


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder d) =
    let
        wrapper ( x, rest ) =
            ( f x, rest )
    in
        Decoder (Result.map wrapper << d)


oneOf : List (Decoder a) -> Decoder a
oneOf options =
    let
        decoder errs options l =
            case options of
                (Decoder next) :: rest ->
                    case next l of
                        Ok x ->
                            Ok x

                        Err e ->
                            decoder (e :: errs) rest l

                [] ->
                    Err ("No options matched " ++ (String.join "\n" errs))
    in
        Decoder (decoder [] options)


controlAtom : (String -> Result String (Decoder a)) -> Decoder (List a)
controlAtom lookup =
    let
        decoder l =
            case l of
                LParen :: (Atom key) :: rest ->
                    Result.map list (lookup key)
                        `Result.andThen`
                            (\(Decoder d) ->
                                (d (LParen :: rest))
                            )

                e ->
                    Err ("Expected (<atom> ...), found " ++ (toString e))
    in
        Decoder decoder
