module Game exposing
    ( Game
    , addEntry
    , deserialise
    , isOver
    , new
    , serialise
    )

import Json.Decode as Decoder
import Json.Encode as Encoder



--type alias Turn =
--    { hint : Maybe String
--    , buildNextMove : Maybe BuildNextMove
--    , currentPlayer : Player
--    }
--
--
--type alias BuildNextMove =
--    String -> String -> NextMove
--
--
--type NextMove
--    = NextMove String String
--
--
--type alias PreviousThing =
--    String


type Game
    = Game Game_


type alias Game_ =
    { before : List PlayerEntry
    , current : Maybe Player -- when the game is over, there's no current player
    , next : List Player
    }


type alias PlayerEntry =
    { player : Player
    , entry : Entry
    }


type alias Entry =
    { hidden : String
    , visible : String
    }


type alias Player =
    String


new : List Player -> Game
new players =
    Game
        { before = []
        , current = List.head players -- "this is misusing it, as Nothing here indicates the end of the game"
        , next = List.drop 1 players
        }


addEntry : Entry -> Game -> Game
addEntry entry (Game game) =
    if List.isEmpty game.next then
        Game
            { before = game.before ++ [ { player = game.current |> Maybe.withDefault "this is starting to smell", entry = entry } ]
            , current = Nothing
            , next = []
            }

    else
        Game
            { before = game.before ++ [ { player = game.current |> Maybe.withDefault "this is starting to smell", entry = entry } ]
            , current = List.head game.next
            , next = List.drop 1 game.next
            }


isOver : Game -> Bool
isOver (Game game) =
    game.current == Nothing



-- to and from a json string


deserialise : String -> Maybe Game
deserialise =
    Decoder.decodeString gameDecoder
        >> Result.toMaybe


gameDecoder : Decoder.Decoder Game
gameDecoder =
    Decoder.map Game
        (Decoder.map3 Game_
            (Decoder.field "before" (Decoder.list playerEntryDecoder))
            (Decoder.field "current" (Decoder.maybe Decoder.string))
            (Decoder.field "next" (Decoder.list Decoder.string))
        )


playerEntryDecoder : Decoder.Decoder PlayerEntry
playerEntryDecoder =
    Decoder.map2 PlayerEntry
        (Decoder.field "player" Decoder.string)
        (Decoder.field "entry" entryDecoder)


entryDecoder : Decoder.Decoder Entry
entryDecoder =
    Decoder.map2 Entry
        (Decoder.field "visible" Decoder.string)
        (Decoder.field "hidden" Decoder.string)


serialise : Game -> String
serialise game =
    Encoder.encode 0 (toObject game)


toObject : Game -> Encoder.Value
toObject (Game game) =
    Encoder.object
        ([ ( "before", encodeBefore game.before )
         , ( "next", Encoder.list Encoder.string game.next )
         ]
            ++ encodeCurrent game
        )


encodeCurrent : Game_ -> List ( String, Encoder.Value )
encodeCurrent =
    .current
        >> Maybe.map (Encoder.string >> Tuple.pair "current" >> List.singleton)
        >> Maybe.withDefault []


encodeBefore : List PlayerEntry -> Encoder.Value
encodeBefore =
    Encoder.list encodeEntry


encodeEntry : PlayerEntry -> Encoder.Value
encodeEntry { player, entry } =
    Encoder.object
        [ ( "player", Encoder.string player )
        , ( "entry"
          , Encoder.object
                [ ( "hidden", Encoder.string entry.hidden )
                , ( "visible", Encoder.string entry.visible )
                ]
          )
        ]
