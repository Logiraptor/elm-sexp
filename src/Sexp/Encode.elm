module Sexp.Encode exposing (..)


type Value
    = Group (List Value)
    | Atom String
    | String String
    | Int Int
