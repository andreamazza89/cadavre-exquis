module Utils.NonEmptyList exposing (..)


type NonEmptyList a
    = NonEmptyList a (List a)


build : a -> List a -> NonEmptyList a
build firstElement otherElements =
    NonEmptyList firstElement otherElements
