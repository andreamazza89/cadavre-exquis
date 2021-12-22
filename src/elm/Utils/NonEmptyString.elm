module Utils.NonEmptyString exposing (NonEmptyString, build, decoder, encode, escapeHatch, get)

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder


type NonEmptyString
    = NonEmptyString String


escapeHatch : NonEmptyString
escapeHatch =
    NonEmptyString "Get rid of this"


build : String -> Maybe NonEmptyString
build s =
    if String.isEmpty s then
        Nothing

    else
        Just (NonEmptyString s)


get : NonEmptyString -> String
get (NonEmptyString s) =
    s



-- to/from JSON


encode : NonEmptyString -> Encoder.Value
encode (NonEmptyString s) =
    Encoder.string s


decoder : Decoder NonEmptyString
decoder =
    Decoder.string
        |> Decoder.andThen
            (\s ->
                case build s of
                    Just nonEmpty ->
                        Decoder.succeed nonEmpty

                    Nothing ->
                        Decoder.fail "was expecting non empty string"
            )
