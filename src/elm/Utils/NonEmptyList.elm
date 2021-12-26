module Utils.NonEmptyList exposing
    ( NonEmptyList
    , append
    , decoder
    , encode
    , fromItem
    , fromList
    , head
    , lastItem
    , tail
    , toList
    )

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder


type NonEmptyList a
    = NonEmptyList a (List a)


fromItem : a -> NonEmptyList a
fromItem firstElement =
    build firstElement []


build : a -> List a -> NonEmptyList a
build firstElement otherElements =
    NonEmptyList firstElement otherElements


fromList : List a -> Maybe (NonEmptyList a)
fromList items =
    case items of
        x :: xs ->
            Just (build x xs)

        _ ->
            Nothing


head : NonEmptyList a -> a
head (NonEmptyList h _) =
    h


tail : NonEmptyList a -> List a
tail (NonEmptyList _ t) =
    t


lastItem : NonEmptyList a -> Maybe a
lastItem nonEmptyList =
    let
        list =
            toList nonEmptyList
    in
    List.drop (List.length list - 1) list
        |> List.head


append : a -> NonEmptyList a -> NonEmptyList a
append item (NonEmptyList h t) =
    NonEmptyList h (t ++ [ item ])


toList : NonEmptyList a -> List a
toList (NonEmptyList h t) =
    h :: t



-- JSONs


encode : (a -> Encoder.Value) -> NonEmptyList a -> Encoder.Value
encode toValue (NonEmptyList h t) =
    Encoder.list toValue (h :: t)


decoder : Decoder a -> Decoder (NonEmptyList a)
decoder valueDecoder =
    Decoder.list valueDecoder
        |> Decoder.andThen
            (fromList
                >> Maybe.map Decoder.succeed
                >> Maybe.withDefault (Decoder.fail "list must not be empty")
            )
