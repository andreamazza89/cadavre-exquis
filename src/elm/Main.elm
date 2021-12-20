module Main exposing (main)

import Browser
import Element exposing (Element, centerX, el, fill, height, minimum, padding, rgb, spacing, width)
import Element.Border as Border
import Element.Font as Font exposing (underline)
import Element.Input as Input exposing (labelHidden, placeholder)
import Game exposing (Game)
import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Url exposing (Url)
import Url.Builder as BuildUrl
import Url.Parser as Url exposing ((</>))
import Url.Parser.Query as Query


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
        |> join
        |> Maybe.map init
        |> Maybe.withDefault defaultModel


join : Maybe (Maybe a) -> Maybe a
join maybes =
    case maybes of
        Just m ->
            m

        Nothing ->
            Nothing


gameParser =
    Url.map parseGame (Url.s "index.html" </> Url.query (Query.string "ongoing-state"))


parseGame =
    Maybe.andThen Game.deserialise


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
            ( { model | step = ShowingUrl game }, Cmd.none )

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
            case Game.status game of
                Game.Playing { hint, currentPlayer } ->
                    Element.link [ padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                        { url = serialiseAsQueryParam game
                        , label =
                            Element.text <| "Copia questo link e mandalo a " ++ currentPlayer
                        }

                Game.LastMove { currentPlayer } ->
                    Element.link [ padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                        { url = serialiseAsQueryParam game
                        , label =
                            Element.text <| "Copia questo link e mandalo a " ++ currentPlayer
                        }

                Game.Ended _ ->
                    Element.link [ padding 100, centerX, underline, Font.color (rgb 0 0 205) ]
                        { url = serialiseAsQueryParam game
                        , label =
                            Element.text <| "Vatti a guardare il cadavere a questo link"
                        }

        Playing game ->
            playingScreen formInputs game


newGameScreen : FormInputs -> Element Msg
newGameScreen formInputs =
    Element.column [ padding 40, spacing 90, centerX ]
        [ welcomeMessage
        , playerrs formInputs
        , startButton formInputs
        ]


welcomeMessage : Element msg
welcomeMessage =
    Element.column [ spacing 5 ]
        [ el
            [ Font.bold, Font.size 44 ]
            (Element.text "Benvenuto a Cadavre Exquis!")
        , el
            [ centerX, Font.size 30 ]
            (Element.text "Aggiungi giocatori per creare una partita")
        ]


playerrs : FormInputs -> Element Msg
playerrs formInputs =
    Element.column [ centerX, spacing 20 ]
        [ Element.row [ spacing 20 ]
            [ Input.text []
                { onChange = PlayerNameTyped
                , text = formInputs.playerName
                , placeholder = Just (placeholder [] (Element.text "Es. Asdrubale"))
                , label = labelHidden "player name"
                }
            , Input.button [ padding 5, height fill, Border.width 2, Border.rounded 2 ]
                { onPress = Just AddPlayerClicked
                , label = Element.text "+"
                }
            ]
        , Element.column [ spacing 15 ]
            (List.map Element.text formInputs.otherPlayers)
        ]


startButton : FormInputs -> Element Msg
startButton formInputs =
    List.head formInputs.otherPlayers
        |> Maybe.andThen
            -- this is kinda terrible, would prefer to either introduce some form validation construct of just use an if statement
            (\firstPlayer ->
                if List.length formInputs.otherPlayers > 1 then
                    Just firstPlayer

                else
                    Nothing
            )
        |> Maybe.map
            (\firstPlayer ->
                Input.button [ centerX, padding 5, height fill, Border.width 2, Border.rounded 2 ]
                    { onPress = Just (StartGameClicked (Game.new firstPlayer (List.drop 1 formInputs.otherPlayers)))
                    , label = Element.text "Entra nella bara..."
                    }
            )
        |> Maybe.withDefault Element.none


playingScreen : FormInputs -> Game -> Element Msg
playingScreen formInput game =
    case Game.status game of
        Game.Playing { hint, currentPlayer } ->
            Element.column [ width fill, centerX, padding 90, spacing 50 ]
                [ prompt currentPlayer
                , Maybe.map Element.text hint |> Maybe.withDefault Element.none
                , yourStory formInput
                , hintToTheNext formInput
                , moveButton game formInput
                ]

        Game.LastMove { hint, currentPlayer } ->
            Element.column [ width fill, centerX, padding 90, spacing 50 ]
                [ prompt currentPlayer
                , Element.text hint
                , yourStory formInput
                , moveButton game formInput
                ]

        Game.Ended { entries, finalEntry } ->
            Element.column []
                (List.map (\entry -> Element.text (entry.hidden ++ entry.visible)) entries
                    ++ [ Element.text finalEntry.hidden ]
                )


prompt player =
    Element.text ("Tocca a te " ++ player)


yourStory formInput =
    Input.multiline [ height (fill |> minimum 300) ]
        { onChange = HiddenEntryTyped
        , text = formInput.hidden
        , placeholder = Nothing
        , label = labelHidden "yourStory"
        , spellcheck = False
        }


hintToTheNext formInput =
    Input.multiline []
        { onChange = VisibleEntryTyped
        , text = formInput.visible
        , placeholder = Just (placeholder [] (Element.text "Qui va il messagio per il prossimo"))
        , label = labelHidden "yourStory"
        , spellcheck = False
        }


moveButton g formInputs =
    if String.isEmpty formInputs.hidden || String.isEmpty formInputs.visible then
        Element.none

    else
        Game.makeNormalMove formInputs.hidden formInputs.visible g
            |> Maybe.map
                (\game_ ->
                    Input.button [ padding 5, height fill, Border.width 2, Border.rounded 2 ]
                        { onPress = Just (ContinueGameClicked game_)
                        , label = Element.text "Procedi"
                        }
                )
            |> Maybe.withDefault Element.none


view { step, formInputs } =
    case step of
        Creating ->
            Html.div []
                [ Html.text "Aggiungi giocatori"
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.placeholder "Nome del giocatore"
                    , Attr.value formInputs.playerName
                    , onInput PlayerNameTyped
                    ]
                    []
                , Html.button [ onClick AddPlayerClicked ] [ Html.text "+" ]
                , Html.div [] (players formInputs.otherPlayers)
                , startGameButton formInputs.otherPlayers
                ]

        ShowingUrl game ->
            case Game.status game of
                Game.Playing { hint, currentPlayer } ->
                    Html.a [ Attr.href (serialiseAsQueryParam game) ]
                        [ Html.text ("send this to " ++ currentPlayer)
                        ]

                Game.LastMove { currentPlayer } ->
                    Html.a [ Attr.href (serialiseAsQueryParam game) ]
                        [ Html.text ("send this to " ++ currentPlayer)
                        ]

                Game.Ended _ ->
                    Html.a [ Attr.href (serialiseAsQueryParam game) ]
                        [ Html.text "send this to everyone"
                        ]

        Playing game ->
            case Game.status game of
                Game.Playing { hint, currentPlayer } ->
                    Html.div []
                        [ Html.div [] [ Html.text ("tocca a te, " ++ currentPlayer) ]
                        , Maybe.map (Html.text >> List.singleton >> Html.div []) hint |> Maybe.withDefault (Html.div [] [])
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.placeholder "nascosto"
                            , Attr.value formInputs.hidden
                            , onInput HiddenEntryTyped
                            ]
                            []
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.placeholder "per il prossimo"
                            , Attr.value formInputs.visible
                            , onInput VisibleEntryTyped
                            ]
                            []
                        , makeMoveButton game formInputs
                        ]

                Game.LastMove { hint, currentPlayer } ->
                    Html.div []
                        [ Html.div [] [ Html.text ("tocca a te, " ++ currentPlayer) ]
                        , Html.div [] [ Html.text hint ]
                        , Html.input
                            [ Attr.type_ "text"
                            , Attr.placeholder "nascosto"
                            , Attr.value formInputs.hidden
                            , onInput HiddenEntryTyped
                            ]
                            []
                        , finishGameButton game formInputs
                        ]

                Game.Ended { entries, finalEntry } ->
                    Html.div []
                        (viewEntries entries ++ viewFinalEntry finalEntry)


viewEntries =
    List.map viewEntry


viewEntry { player, visible, hidden } =
    Html.div [] [ Html.text (player ++ ": " ++ hidden ++ " " ++ visible) ]


viewFinalEntry { player, hidden } =
    [ Html.div [] [ Html.text (player ++ ": " ++ hidden) ] ]


makeMoveButton : Game -> FormInputs -> Html.Html Msg
makeMoveButton g formInputs =
    if String.isEmpty formInputs.hidden || String.isEmpty formInputs.visible then
        Html.div [] []

    else
        Game.makeNormalMove formInputs.hidden formInputs.visible g
            |> Maybe.map (\game_ -> Html.button [ onClick (ContinueGameClicked game_) ] [ Html.text "continua" ])
            |> Maybe.withDefault (Html.div [] [])


finishGameButton : Game -> FormInputs -> Html.Html Msg
finishGameButton g formInputs =
    if String.isEmpty formInputs.hidden then
        Html.div [] []

    else
        Game.makeLastMove formInputs.hidden g
            |> Maybe.map (\game_ -> Html.button [ onClick (ContinueGameClicked game_) ] [ Html.text "concludi" ])
            |> Maybe.withDefault (Html.div [] [])


startGameButton : List String -> Html.Html Msg
startGameButton players_ =
    List.head players_
        |> Maybe.map
            (\firstPlayer ->
                Html.div [] [ Html.button [ onClick (StartGameClicked (Game.new firstPlayer (List.drop 1 players_))) ] [ Html.text "Comincia" ] ]
            )
        |> Maybe.withDefault (Html.div [] [])


serialiseAsQueryParam : Game -> String
serialiseAsQueryParam game =
    BuildUrl.absolute [ "index.html" ] [ BuildUrl.string "ongoing-state" (Game.serialise game) ]


players : List String -> List (Html.Html msg)
players otherPlayers =
    List.map (Html.text >> List.singleton >> Html.div []) otherPlayers
