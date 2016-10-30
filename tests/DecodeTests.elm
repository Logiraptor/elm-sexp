module DecodeTests exposing (..)

import Test
import Expect
import Sexp.Decode
import Sexp.Lexer
import LexerTests


all : Test.Test
all =
    Test.describe "Sexp.Decode"
        [ Test.describe "end to end"
            endToEnd
        , Test.describe "lexer"
            LexerTests.all
        ]


type alias TestCase a =
    { input : String
    , output : Result String a
    , decoder : Sexp.Decode.Decoder a
    }


check : TestCase a -> Test.Test
check tc =
    Test.test tc.input (\() -> Expect.equal tc.output (Sexp.Decode.decodeString tc.decoder tc.input))


endToEnd : List Test.Test
endToEnd =
    [ TestCase "\"abc\"" (Ok "abc") Sexp.Decode.string |> check
    , TestCase "5" (Ok 5) Sexp.Decode.int |> check
    , TestCase "2.3" (Ok 2.3) Sexp.Decode.float |> check
    , TestCase "()" (Ok ()) Sexp.Decode.nil |> check
    , TestCase "(1 2 3)" (Ok [ 1, 2, 3 ]) (Sexp.Decode.list Sexp.Decode.int) |> check
    , TestCase "3" (Ok 3.0) (Sexp.Decode.map toFloat Sexp.Decode.int) |> check
    , TestCase "(\"a\" 1)"
        (Ok [ S "a", I 1 ])
        (Sexp.Decode.list
            (Sexp.Decode.oneOf
                [ Sexp.Decode.map I Sexp.Decode.int
                , Sexp.Decode.map S Sexp.Decode.string
                ]
            )
        )
        |> check
    , TestCase "((I 5) (S \"bar\"))"
        (Ok [ [ I 5 ], [ S "bar" ] ])
        (Sexp.Decode.list
            (Sexp.Decode.controlAtom
                (\s ->
                    case s of
                        "S" ->
                            Ok (Sexp.Decode.map S Sexp.Decode.string)

                        "I" ->
                            Ok (Sexp.Decode.map I Sexp.Decode.int)

                        _ ->
                            Err ""
                )
            )
        )
        |> check
    ]


type StringOrInt
    = S String
    | I Int
