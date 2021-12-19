module Game exposing
    ( Game
    , Status(..)
    , deserialise
    , makeLastMove
    , makeNormalMove
    , new
    , serialise
    , status
    )

import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder


type Game
    = Ongoing OngoingGame_
    | Finished FinishedGame_


type alias OngoingGame_ =
    { before : List PlayerEntry
    , current : Player
    , next : List Player
    }


type alias FinishedGame_ =
    { entries : List PlayerEntry
    , finalEntry : FinalPlayerEntry
    }


type alias PlayerEntry =
    { player : Player
    , visible : String
    , hidden : String
    }


type alias FinalPlayerEntry =
    { player : Player
    , hidden : String
    }


type alias Player =
    String


new : Player -> List Player -> Game
new currentPlayer_ otherPlayers =
    Ongoing
        { before = []
        , current = currentPlayer_
        , next = otherPlayers
        }


type Status
    = Playing { hint : Maybe String, currentPlayer : Player }
    | LastMove { hint : String, currentPlayer : Player }
    | Ended { entries : List PlayerEntry, finalEntry : FinalPlayerEntry }


status : Game -> Status
status game =
    case game of
        Ongoing ongoing_ ->
            if List.isEmpty ongoing_.next then
                LastMove
                    { hint = lastEntry ongoing_.before |> Maybe.map .visible |> Maybe.withDefault "this should not happen"
                    , currentPlayer = ongoing_.current
                    }

            else
                Playing { hint = lastEntry ongoing_.before |> Maybe.map .visible, currentPlayer = ongoing_.current }

        Finished finished_ ->
            Ended finished_


lastEntry : List a -> Maybe a
lastEntry before =
    List.drop (List.length before - 1) before
        |> List.head


makeLastMove : String -> Game -> Maybe Game
makeLastMove hidden game =
    case game of
        Ongoing ongoingGame_ ->
            if List.isEmpty ongoingGame_.next then
                Just
                    (Finished
                        { entries = ongoingGame_.before
                        , finalEntry = { player = ongoingGame_.current, hidden = hidden }
                        }
                    )

            else
                Nothing

        Finished _ ->
            Nothing


makeNormalMove : String -> String -> Game -> Maybe Game
makeNormalMove hidden visible game =
    case game of
        Ongoing ongoingGame_ ->
            List.head ongoingGame_.next
                |> Maybe.map
                    (\nextPlayer ->
                        Ongoing
                            { before = ongoingGame_.before ++ [ { player = ongoingGame_.current, visible = visible, hidden = hidden } ]
                            , current = nextPlayer
                            , next = List.drop 1 ongoingGame_.next
                            }
                    )

        Finished _ ->
            Nothing



-- to and from a json string


deserialise : String -> Maybe Game
deserialise =
    Decoder.decodeString gameDecoder
        >> Debug.log "result"
        >> Result.toMaybe


gameDecoder : Decoder.Decoder Game
gameDecoder =
    Decoder.field "type" Decoder.string
        |> Decoder.andThen
            (\t ->
                case t of
                    "ONGOING" ->
                        Decoder.map Ongoing ongoingDecoder

                    "FINISHED" ->
                        Decoder.map Finished finishedDecoder

                    _ ->
                        Decoder.fail "unknown type"
            )


ongoingDecoder : Decoder OngoingGame_
ongoingDecoder =
    Decoder.map3 OngoingGame_
        (Decoder.field "before" (Decoder.list playerEntryDecoder))
        (Decoder.field "current" Decoder.string)
        (Decoder.field "next" (Decoder.list Decoder.string))


finishedDecoder =
    Decoder.map2 FinishedGame_
        (Decoder.field "entries" (Decoder.list playerEntryDecoder))
        (Decoder.field "finalEntry" finalEntryDecoder)


playerEntryDecoder : Decoder PlayerEntry
playerEntryDecoder =
    Decoder.map3 PlayerEntry
        (Decoder.field "player" Decoder.string)
        (Decoder.field "visible" Decoder.string)
        (Decoder.field "hidden" Decoder.string)


finalEntryDecoder : Decoder FinalPlayerEntry
finalEntryDecoder =
    Decoder.map2 FinalPlayerEntry
        (Decoder.field "player" Decoder.string)
        (Decoder.field "hidden" Decoder.string)


serialise : Game -> String
serialise game =
    Encoder.encode 0 (toObject game)


toObject : Game -> Encoder.Value
toObject game =
    case game of
        Ongoing ongoing_ ->
            Encoder.object
                [ ( "type", Encoder.string "ONGOING" )
                , ( "current", Encoder.string ongoing_.current )
                , ( "before", encodeBefore ongoing_.before )
                , ( "next", Encoder.list Encoder.string ongoing_.next )
                ]

        Finished finished_ ->
            Encoder.object
                [ ( "type", Encoder.string "FINISHED" )
                , ( "entries", encodeBefore finished_.entries )
                , ( "finalEntry", encodeFinal finished_.finalEntry )
                ]


encodeFinal : FinalPlayerEntry -> Encoder.Value
encodeFinal { player, hidden } =
    Encoder.object
        [ ( "player", Encoder.string player )
        , ( "hidden", Encoder.string hidden )
        ]


encodeBefore : List PlayerEntry -> Encoder.Value
encodeBefore =
    Encoder.list encodeEntry


encodeEntry : PlayerEntry -> Encoder.Value
encodeEntry { player, visible, hidden } =
    Encoder.object
        [ ( "player", Encoder.string player )
        , ( "hidden", Encoder.string hidden )
        , ( "visible", Encoder.string visible )
        ]
