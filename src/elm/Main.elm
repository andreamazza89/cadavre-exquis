module Main exposing (main)

import Browser
import Element exposing (Element, centerX, el, fill, height, minimum, padding, rgb, spacing, width)
import Element.Border as Border
import Element.Font as Font exposing (underline)
import Element.Input as Input exposing (focusedOnLoad, labelHidden, placeholder)
import Game exposing (Game)
import Url exposing (Url)
import Url.Builder as BuildUrl
import Url.Parser as Url exposing ((</>))
import Url.Parser.Query as Query
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
    { otherPlayers : List String
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
        , view = \st -> { title = "Cadavre", body = [ Element.layout [] (view2 st) ] }
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


initFromUrl : Url -> Model
initFromUrl url =
    Url.parse gameParser url
        |> joinMaybes
        |> Maybe.map init
        |> Maybe.withDefault defaultModel


joinMaybes : Maybe (Maybe a) -> Maybe a
joinMaybes maybes =
    case maybes of
        Just m ->
            m

        Nothing ->
            Nothing


gameParser =
    Url.map parseGame (Url.s "index.html" </> Url.query (Query.string "ongoing-state"))


parseGame =
    Maybe.map (String.map decode)
        >> Maybe.andThen Game.deserialise


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
    { inputs
        | playerName = ""
        , otherPlayers = [ inputs.playerName ] ++ inputs.otherPlayers
    }


view2 : Model -> Element Msg
view2 { step, formInputs } =
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
            (List.map Element.text formInputs.otherPlayers)
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
        |> Validation.require (.otherPlayers >> List.tail)
        |> Validation.checkIsTrue (.otherPlayers >> List.length >> (\len -> len > 1))


urlScreen : Game -> Element msg
urlScreen game =
    case Game.status game of
        Game.Playing { hint, currentPlayer } ->
            Element.link [ Font.size 40, padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                { url = serialiseAsQueryParam game
                , label =
                    Element.text <| "Copia questo link e mandalo a " ++ currentPlayer
                }

        Game.LastMove { currentPlayer } ->
            Element.link [ Font.size 40, padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                { url = serialiseAsQueryParam game
                , label =
                    Element.text <| "Copia questo link e mandalo a " ++ currentPlayer
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
            Element.column [ Font.size 30, width fill, centerX, padding 90, spacing 50 ]
                [ prompt currentPlayer
                , Maybe.map Element.text hint |> Maybe.withDefault Element.none
                , yourStory formInput
                , hintToTheNext formInput
                , moveButton "Continua" formInput (normalMoveValidation game)
                ]

        Game.LastMove { hint, currentPlayer } ->
            Element.column [ Font.size 30, width fill, centerX, padding 90, spacing 50 ]
                [ prompt currentPlayer
                , Element.text hint
                , yourStory formInput
                , moveButton "Concludi" formInput (lastMoveValidation game)
                ]

        Game.Ended { entries, finalEntry } ->
            Element.column [ padding 60, spacing 20, Font.size 40 ]
                (List.map (\entry -> Element.text (entry.hidden ++ " " ++ entry.visible)) entries
                    ++ [ Element.text finalEntry.hidden ]
                )


prompt : String -> Element msg
prompt player =
    el [ Font.size 30 ] <| Element.text ("Tocca a te " ++ player)


yourStory : FormInputs -> Element Msg
yourStory formInput =
    Input.multiline [ Font.size 30, height (fill |> minimum 300) ]
        { onChange = HiddenEntryTyped
        , text = formInput.hidden
        , placeholder = Just (placeholder [] (Element.text "Qui va la tua storia"))
        , label = labelHidden "yourStory"
        , spellcheck = False
        }


hintToTheNext : FormInputs -> Element Msg
hintToTheNext formInput =
    Input.multiline [ Font.size 30 ]
        { onChange = VisibleEntryTyped
        , text = formInput.visible
        , placeholder = Just (placeholder [] (Element.text "Qui va il messagio per il prossimo"))
        , label = labelHidden "yourStory"
        , spellcheck = False
        }


moveButton : String -> FormInputs -> Validation FormInputs (Maybe Game) -> Element Msg
moveButton label inputs validation =
    Validation.run inputs validation
        |> joinMaybes
        |> Maybe.map (moveButton_ label)
        |> Maybe.withDefault Element.none


moveButton_ : String -> Game -> Element Msg
moveButton_ label game =
    Input.button [ Font.size 30, padding 5, height fill, Border.width 2, Border.rounded 2 ]
        { onPress = Just (ContinueGameClicked game)
        , label = Element.text label
        }


normalMoveValidation : Game -> Validation FormInputs (Maybe Game)
normalMoveValidation game =
    Validation.for (\hidden visible -> Game.makeNormalMove hidden visible game)
        |> Validation.require (.hidden >> Just)
        |> Validation.require (.visible >> Just)


lastMoveValidation : Game -> Validation FormInputs (Maybe Game)
lastMoveValidation game =
    Validation.for (\hidden -> Game.makeLastMove hidden game)
        |> Validation.require (.hidden >> Just)


serialiseAsQueryParam : Game -> String
serialiseAsQueryParam game =
    BuildUrl.absolute [ "index.html" ] [ BuildUrl.string "ongoing-state" (String.map encode <| Game.serialise game) ]


encode : Char -> Char
encode a =
    Char.toCode a |> (\c -> c + 1 |> Char.fromCode)


decode : Char -> Char
decode a =
    Char.toCode a |> (\c -> c - 1 |> Char.fromCode)
