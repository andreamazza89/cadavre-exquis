module Main exposing (main)

import Browser
import Element exposing (Element, centerX, el, fill, height, minimum, padding, px, rgb, spacing, width)
import Element.Border as Border
import Element.Font as Font exposing (underline)
import Element.Input as Input exposing (focusedOnLoad, labelHidden, placeholder)
import Game exposing (Game)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)
import Url.Builder as BuildUrl
import Url.Parser as Url exposing ((</>))
import Url.Parser.Query as Query
import Utils.NonEmptyList as NonEmptyList exposing (NonEmptyList)
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)
import Validation exposing (Validation)


type alias Model =
    { step : Step
    , formInputs : FormInputs
    }


type Step
    = Creating
    | Playing Game
    | ShowingUrl Game


type Msg
    = PlayerNameTyped String
    | AddPlayerClicked
    | StartGameClicked Game
    | VisibleEntryTyped String
    | HiddenEntryTyped String
    | ContinueGameClicked Game
    | NoOp


type alias FormInputs =
    { otherPlayers : List NonEmptyString
    , playerName : String
    , visible : String
    , hidden : String
    }


initialForm : FormInputs
initialForm =
    { otherPlayers = []
    , playerName = ""
    , visible = ""
    , hidden = ""
    }


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url _ -> ( initFromUrl url, Cmd.none )
        , view = \st -> { title = "Cadavre", body = [ Element.layout [] (view st) ] }
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


joinMaybes : Maybe (Maybe a) -> Maybe a
joinMaybes maybes =
    -- move me to a util space (or get rid of me if you can)
    case maybes of
        Just m ->
            m

        Nothing ->
            Nothing


init : Game -> Model
init game =
    { formInputs = initialForm
    , step = Playing game
    }


defaultModel : Model
defaultModel =
    { formInputs = initialForm
    , step = Creating
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PlayerNameTyped name ->
            ( { model | formInputs = updatePlayerName name model.formInputs }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        AddPlayerClicked ->
            ( { model | formInputs = addPlayer model.formInputs }, Cmd.none )

        StartGameClicked game ->
            ( { model | step = Playing game }, Cmd.none )

        VisibleEntryTyped visible ->
            ( { model | formInputs = updateVisibleEntry visible model.formInputs }, Cmd.none )

        HiddenEntryTyped visible ->
            ( { model | formInputs = updateHiddenEntry visible model.formInputs }, Cmd.none )

        ContinueGameClicked game ->
            ( { model | step = ShowingUrl game }, Cmd.none )


updateVisibleEntry : String -> FormInputs -> FormInputs
updateVisibleEntry text inputs =
    { inputs | visible = text }


updateHiddenEntry : String -> FormInputs -> FormInputs
updateHiddenEntry text inputs =
    { inputs | hidden = text }


updatePlayerName : String -> FormInputs -> FormInputs
updatePlayerName name inputs =
    { inputs | playerName = name }


addPlayer : FormInputs -> FormInputs
addPlayer inputs =
    Maybe.map (addPlayer_ inputs) (NonEmptyString.build inputs.playerName)
        |> Maybe.withDefault inputs


addPlayer_ : FormInputs -> NonEmptyString -> FormInputs
addPlayer_ inputs playerToAdd =
    { inputs
        | playerName = ""
        , otherPlayers = [ playerToAdd ] ++ inputs.otherPlayers
    }


view : Model -> Element Msg
view { step, formInputs } =
    case step of
        Creating ->
            newGameScreen formInputs

        ShowingUrl game ->
            urlScreen game

        Playing game ->
            playingScreen formInputs game


newGameScreen : FormInputs -> Element Msg
newGameScreen formInputs =
    Element.column [ padding 40, spacing 90, centerX ]
        [ welcomeMessage
        , players formInputs
        , startButton formInputs
        ]


welcomeMessage : Element msg
welcomeMessage =
    Element.column [ spacing 5 ]
        [ el
            [ Font.bold, Font.size 50 ]
            (Element.text "Benvenuto a Cadavre Exquis!")
        , el
            [ centerX, Font.size 30 ]
            (Element.text "Aggiungi giocatori per creare una partita")
        ]


players : FormInputs -> Element Msg
players formInputs =
    Element.column [ centerX, spacing 20 ]
        [ Element.row [ spacing 20 ]
            [ Input.text [ focusedOnLoad, Font.size 30 ]
                { onChange = PlayerNameTyped
                , text = formInputs.playerName
                , placeholder = Just (placeholder [] (Element.text "Es. Asdrubale"))
                , label = labelHidden "player name"
                }
            , Input.button [ padding 15, height fill, Border.width 2, Border.rounded 2 ]
                { onPress = Just AddPlayerClicked
                , label = Element.text "+"
                }
            ]
        , Element.column [ spacing 15, Font.size 30 ]
            (List.map Element.text (List.map NonEmptyString.get formInputs.otherPlayers))
        ]


startButton : FormInputs -> Element Msg
startButton inputs =
    Validation.run inputs startButtonValidation
        |> Maybe.map startButton_
        |> Maybe.withDefault Element.none


startButton_ : Game -> Element Msg
startButton_ game =
    Input.button [ Font.size 30, centerX, padding 5, height fill, Border.width 2, Border.rounded 2 ]
        { onPress = Just (StartGameClicked game)
        , label = Element.text "Entra nella bara..."
        }


startButtonValidation : Validation FormInputs Game
startButtonValidation =
    Validation.for Game.new
        |> Validation.require (.otherPlayers >> List.head)
        |> Validation.require (.otherPlayers >> List.drop 1 >> NonEmptyList.fromList)


urlScreen : Game -> Element msg
urlScreen game =
    case Game.status game of
        Game.Playing { hint, currentPlayer } ->
            Element.link [ Font.size 40, padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                { url = serialiseAsQueryParam game
                , label =
                    Element.text <| "Copia questo link e mandalo a " ++ NonEmptyString.get currentPlayer
                }

        Game.LastMove { currentPlayer } ->
            Element.link [ Font.size 40, padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                { url = serialiseAsQueryParam game
                , label =
                    Element.text <| "Copia questo link e mandalo a " ++ NonEmptyString.get currentPlayer
                }

        Game.Ended _ ->
            Element.link [ Font.size 40, padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                { url = serialiseAsQueryParam game
                , label =
                    Element.text <| "Vatti a guardare il cadavere a questo link"
                }


playingScreen : FormInputs -> Game -> Element Msg
playingScreen formInput game =
    case Game.status game of
        Game.Playing { hint, currentPlayer } ->
            paper
                { currentPlayer = currentPlayer
                , hint = hint
                , showVisible = True
                , button = mveButton "Continua" formInput (normalMoveValidation game)
                }

        Game.LastMove { hint, currentPlayer } ->
            paper
                { currentPlayer = currentPlayer
                , hint = Just hint
                , showVisible = False
                , button = mveButton "Concludi" formInput (lastMoveValidation game)
                }

        Game.Ended { entries, finalEntry } ->
            Element.paragraph [ padding 60, spacing 20, Font.size 40 ]
                (List.map (\entry -> Element.text (NonEmptyString.get entry.hidden ++ " " ++ NonEmptyString.get entry.visible)) (NonEmptyList.toList entries)
                    ++ [ Element.text (NonEmptyString.get finalEntry.hidden) ]
                )


mveButton : String -> FormInputs -> Validation FormInputs (Maybe Game) -> Maybe ( Msg, String )
mveButton label inputs validation =
    Validation.run inputs validation
        |> joinMaybes
        |> Maybe.map (\game -> ( ContinueGameClicked game, label ))


normalMoveValidation : Game -> Validation FormInputs (Maybe Game)
normalMoveValidation game =
    Validation.for (\hidden visible -> Game.makeNormalMove hidden visible game)
        |> Validation.require (.hidden >> NonEmptyString.build)
        |> Validation.require (.visible >> NonEmptyString.build)


lastMoveValidation : Game -> Validation FormInputs (Maybe Game)
lastMoveValidation game =
    Validation.for (\hidden -> Game.makeLastMove hidden game)
        |> Validation.require (.hidden >> NonEmptyString.build)



-- Game to/from url stuff


initFromUrl : Url -> Model
initFromUrl url =
    Url.parse gameParser url
        |> joinMaybes
        |> Maybe.map init
        |> Maybe.withDefault defaultModel


gameParser : Url.Parser (Maybe Game -> c) c
gameParser =
    Url.map parseGame (Url.s "index.html" </> Url.query (Query.string "ongoing-state"))


parseGame : Maybe String -> Maybe Game
parseGame =
    Maybe.map (String.map decrypt)
        >> Maybe.andThen Game.deserialise


serialiseAsQueryParam : Game -> String
serialiseAsQueryParam game =
    BuildUrl.absolute [ "index.html" ] [ BuildUrl.string "ongoing-state" (String.map encrypt <| Game.serialise game) ]


encrypt : Char -> Char
encrypt a =
    Char.toCode a |> (\c -> c + 1 |> Char.fromCode)


decrypt : Char -> Char
decrypt a =
    Char.toCode a |> (\c -> c - 1 |> Char.fromCode)



-- Web component thing


type alias PaperOptions =
    { currentPlayer : NonEmptyString
    , hint : Maybe NonEmptyString
    , showVisible : Bool
    , button : Maybe ( Msg, String )
    }


paper : PaperOptions -> Element Msg
paper options =
    Element.html
        (Html.node "paper-note"
            (List.concat
                [ paperCurrentPlayer options.currentPlayer
                , paperHint options.hint
                , paperHidden
                , paperVisible options.showVisible
                , paperButton options.button
                ]
            )
            []
        )


paperCurrentPlayer : NonEmptyString -> List (Html.Attribute msg)
paperCurrentPlayer player =
    [ Html.Attributes.attribute "current-player" (NonEmptyString.get player) ]


paperHint : Maybe NonEmptyString -> List (Html.Attribute msg)
paperHint =
    Maybe.map
        (\hint_ ->
            [ Html.Attributes.attribute "show-hint" "true"
            , Html.Attributes.attribute "hint-content" (NonEmptyString.get hint_)
            ]
        )
        >> Maybe.withDefault [ Html.Attributes.attribute "show-hint" "false" ]


paperHidden : List (Html.Attribute Msg)
paperHidden =
    [ Html.Events.on "hidden-content-changed" (Decode.map HiddenEntryTyped (Decode.field "detail" Decode.string)) ]


paperVisible : Bool -> List (Html.Attribute Msg)
paperVisible show =
    [ Html.Attributes.attribute "show-visible" (toBooleanString show)
    , Html.Events.on "visible-content-changed" (Decode.map VisibleEntryTyped (Decode.field "detail" Decode.string))
    ]


paperButton : Maybe ( msg, String ) -> List (Html.Attribute msg)
paperButton =
    Maybe.map
        (\( msg, text ) ->
            [ Html.Attributes.attribute "button-text" text
            , Html.Attributes.attribute "show-button" "true"
            , Html.Events.on "button-clicked" (Decode.succeed msg)
            ]
        )
        >> Maybe.withDefault []


toBooleanString : Bool -> String
toBooleanString bool =
    if bool then
        "true"

    else
        "false"
