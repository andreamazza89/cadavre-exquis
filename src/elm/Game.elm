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
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Game
    = JustStarted JustStartedGame_
    | Ongoing OngoingGame_
    | Finished FinishedGame_


type alias JustStartedGame_ =
    { current : Player
    , next : List Player
    }


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
    , visible : NonEmptyString
    , hidden : NonEmptyString
    }


type alias FinalPlayerEntry =
    { player : Player
    , hidden : NonEmptyString
    }


type alias Player =
    NonEmptyString


new : Player -> List Player -> Game
new currentPlayer_ otherPlayers =
    Ongoing
        { before = []
        , current = currentPlayer_
        , next = otherPlayers
        }


type Status
    = Playing { hint : Maybe NonEmptyString, currentPlayer : Player }
    | LastMove { hint : NonEmptyString, currentPlayer : Player }
    | Ended { entries : List PlayerEntry, finalEntry : FinalPlayerEntry }


status : Game -> Status
status game =
    case game of
        JustStarted justStarted_ ->
            Playing
                { hint = Nothing
                , currentPlayer = justStarted_.current
                }

        Ongoing ongoing_ ->
            if List.isEmpty ongoing_.next then
                LastMove
                    { hint =
                        lastEntry ongoing_.before
                            |> Maybe.map .visible
                            |> Maybe.withDefault NonEmptyString.escapeHatch
                    , currentPlayer = ongoing_.current
                    }

            else
                Playing
                    { hint =
                        lastEntry ongoing_.before
                            |> Maybe.map .visible
                    , currentPlayer = ongoing_.current
                    }

        Finished finished_ ->
            Ended finished_


lastEntry : List a -> Maybe a
lastEntry before =
    List.drop (List.length before - 1) before
        |> List.head


makeLastMove : NonEmptyString -> Game -> Maybe Game
makeLastMove hidden game =
    case game of
        JustStarted _ ->
            Nothing

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


makeNormalMove : NonEmptyString -> NonEmptyString -> Game -> Maybe Game
makeNormalMove hidden visible game =
    case game of
        JustStarted justStartedGame_ ->
            List.head justStartedGame_.next
                |> Maybe.map
                    (\nextPlayer ->
                        Ongoing
                            { before = [ { player = justStartedGame_.current, visible = visible, hidden = hidden } ]
                            , current = nextPlayer
                            , next = List.drop 1 justStartedGame_.next
                            }
                    )

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
                    "JUST_STARTED" ->
                        Decoder.map JustStarted justStartedDecoder

                    "ONGOING" ->
                        Decoder.map Ongoing ongoingDecoder

                    "FINISHED" ->
                        Decoder.map Finished finishedDecoder

                    _ ->
                        Decoder.fail "unknown type"
            )


justStartedDecoder : Decoder JustStartedGame_
justStartedDecoder =
    Decoder.map2 JustStartedGame_
        (Decoder.field "current" NonEmptyString.decoder)
        (Decoder.field "next" (Decoder.list NonEmptyString.decoder))


ongoingDecoder : Decoder OngoingGame_
ongoingDecoder =
    Decoder.map3 OngoingGame_
        (Decoder.field "before" (Decoder.list playerEntryDecoder))
        (Decoder.field "current" NonEmptyString.decoder)
        (Decoder.field "next" (Decoder.list NonEmptyString.decoder))


finishedDecoder =
    Decoder.map2 FinishedGame_
        (Decoder.field "entries" (Decoder.list playerEntryDecoder))
        (Decoder.field "finalEntry" finalEntryDecoder)


playerEntryDecoder : Decoder PlayerEntry
playerEntryDecoder =
    Decoder.map3 PlayerEntry
        (Decoder.field "player" NonEmptyString.decoder)
        (Decoder.field "visible" NonEmptyString.decoder)
        (Decoder.field "hidden" NonEmptyString.decoder)


finalEntryDecoder : Decoder FinalPlayerEntry
finalEntryDecoder =
    Decoder.map2 FinalPlayerEntry
        (Decoder.field "player" NonEmptyString.decoder)
        (Decoder.field "hidden" NonEmptyString.decoder)


serialise : Game -> String
serialise game =
    Encoder.encode 0 (toObject game)


toObject : Game -> Encoder.Value
toObject game =
    case game of
        JustStarted justStarted_ ->
            Encoder.object
                [ ( "type", Encoder.string "JUST_STARTED" )
                , ( "current", NonEmptyString.encode justStarted_.current )
                , ( "next", Encoder.list NonEmptyString.encode justStarted_.next )
                ]

        Ongoing ongoing_ ->
            Encoder.object
                [ ( "type", Encoder.string "ONGOING" )
                , ( "current", NonEmptyString.encode ongoing_.current )
                , ( "before", encodeBefore ongoing_.before )
                , ( "next", Encoder.list NonEmptyString.encode ongoing_.next )
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
        [ ( "player", NonEmptyString.encode player )
        , ( "hidden", NonEmptyString.encode hidden )
        ]


encodeBefore : List PlayerEntry -> Encoder.Value
encodeBefore =
    Encoder.list encodeEntry


encodeEntry : PlayerEntry -> Encoder.Value
encodeEntry { player, visible, hidden } =
    Encoder.object
        [ ( "player", NonEmptyString.encode player )
        , ( "hidden", NonEmptyString.encode hidden )
        , ( "visible", NonEmptyString.encode visible )
        ]
