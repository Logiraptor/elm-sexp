module Sexp.Lexer exposing (..)

import String


type Token
    = LParen
    | RParen
    | Atom String
    | Int Int
    | Float Float
    | String String
    | Whitespace
    | EOF


lex : String -> Result String (List Token)
lex s =
    String.toList s
        |> lexChars
        |> Result.map (List.filter ((/=) Whitespace))


lexChars : List Char -> Result String (List Token)
lexChars c =
    let
        continue ( token, rest ) =
            case token of
                EOF ->
                    Ok [ token ]

                _ ->
                    Result.map ((::) token) (lexChars rest)
    in
        nextToken c
            `Result.andThen` continue


nextToken : List Char -> Result String ( Token, List Char )
nextToken chars =
    case chars of
        [] ->
            Ok ( EOF, [] )

        h :: t ->
            if h == '(' then
                Ok ( LParen, t )
            else if h == ')' then
                Ok ( RParen, t )
            else if h == ' ' then
                Ok ( Whitespace, t )
            else if isDigit h then
                let
                    floatResult =
                        lexFloat (h :: t)

                    ( match, rest ) =
                        consumeWhile isDigit chars

                    toToken ( match, rest ) =
                        if List.any ((==) '.') match then
                            ( match
                                |> String.fromList
                                |> String.toFloat
                                |> Result.withDefault 0
                                |> Float
                            , rest
                            )
                        else
                            ( match
                                |> String.fromList
                                |> String.toInt
                                |> Result.withDefault 0
                                |> Int
                            , rest
                            )
                in
                    Result.map toToken floatResult
            else if isLetter h then
                let
                    ( match, rest ) =
                        consumeWhile isLetter chars
                in
                    Ok
                        ( match
                            |> String.fromList
                            |> Atom
                        , rest
                        )
            else if isQuote h then
                let
                    inner ( match, rest ) =
                        ( match |> String.fromList |> String, rest )
                in
                    Result.map inner (lexString t)
            else
                Err ("Unrecognized character: " ++ (toString h))


lexFloat : List Char -> Result String ( List Char, List Char )
lexFloat chars =
    let
        ( left, rest ) =
            consumeWhile isDigit chars
    in
        case rest of
            '.' :: end ->
                let
                    ( right, end ) =
                        consumeWhile isDigit end
                in
                    Ok ( (left ++ [ '.' ] ++ right), end )

            _ ->
                Ok ( left, rest )


lexString : List Char -> Result String ( List Char, List Char )
lexString chars =
    let
        ( match, rest ) =
            consumeWhile (not << isQuote) chars
    in
        case rest of
            '"' :: '"' :: rest ->
                let
                    r =
                        lexString rest

                    output ( endOfString, realRest ) =
                        ( match ++ ('"' :: endOfString), realRest )
                in
                    Result.map output r

            '"' :: rest ->
                Ok
                    ( match
                    , rest
                    )

            h :: _ ->
                Err ("Expected '\"', found " ++ (toString h))

            [] ->
                Err ("Unexpected EOF in string literal")


isQuote : Char -> Bool
isQuote c =
    c == '"'


isDigit : Char -> Bool
isDigit c =
    c >= '0' && c <= '9'


isLetter : Char -> Bool
isLetter c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')


consumeWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
consumeWhile predicate l =
    case l of
        [] ->
            ( [], [] )

        h :: t ->
            if predicate h then
                let
                    ( furtherMatch, rest ) =
                        consumeWhile predicate t
                in
                    ( h :: furtherMatch, rest )
            else
                ( [], l )
