module Main exposing (..)

import DecodeTests
import Test.Runner.Html


main : Program Never
main =
    Test.Runner.Html.run DecodeTests.all
