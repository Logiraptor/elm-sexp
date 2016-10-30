module LexerTests exposing (..)

import Test
import Expect
import Sexp.Decode
import Sexp.Lexer exposing (..)


all : List Test.Test
all =
    let
        name input =
            "(lex `" ++ input ++ "`)"

        lexTest ( input, output ) =
            Test.test (name input) (\() -> Expect.equal (output) (Sexp.Lexer.lex input))
    in
        List.map lexTest
            [ ( "5", Ok [ Int 5, EOF ] )
            , ( "2.5", Ok [ Float 2.5, EOF ] )
            , ( "abc", Ok [ Atom "abc", EOF ] )
            , ( "(", Ok [ LParen, EOF ] )
            , ( ")", Ok [ RParen, EOF ] )
            , ( "\"\"", Ok [ String "", EOF ] )
            , ( "\"contents\"", Ok [ String "contents", EOF ] )
            , ( "\"cont\"\"ents\"", Ok [ String "cont\"ents", EOF ] )
            , ( "(if (a) \"foo\" 12)", Ok [ LParen, Atom "if", LParen, Atom "a", RParen, String "foo", Int 12, RParen, EOF ] )
            ]
