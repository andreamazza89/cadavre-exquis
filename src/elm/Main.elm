module Main exposing (main)

import Browser
import Game exposing (Game)
import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Url exposing (Url)
import Url.Builder as BuildUrl
import Url.Parser as Url
import Url.Parser.Query as Query


type alias Model =
    { step : Step
    , formInputs : FormInputs
    }


type Step
    = Creating
    | Playing Game
    | ShowingUrl Game
    | ShowingCadaver Game


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
        , view = \st -> { title = "Cadavre", body = [ view st ] }
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
    Url.map parseGame (Url.query (Query.string "ongoing-state"))


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
            if Game.isOver game then
                ( { model | step = ShowingCadaver game }, Cmd.none )

            else
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
                , Html.div [] [ Html.button [ onClick (StartGameClicked (Game.new formInputs.otherPlayers)) ] [ Html.text "Comincia" ] ]
                ]

        ShowingUrl game ->
            Html.a [ Attr.href (serialiseAsQueryParam game) ]
                [ Html.text "send this to friend"
                ]

        Playing game ->
            Html.div []
                [ Html.text "Aggiungi  la tua storia"
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.placeholder "la tua storia"
                    , Attr.value formInputs.visible
                    , onInput VisibleEntryTyped
                    ]
                    []
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.placeholder "il cadavere per il prossimo giocatore"
                    , Attr.value formInputs.hidden
                    , onInput HiddenEntryTyped
                    ]
                    []
                , Html.div [] [ Html.button [ onClick (ContinueGameClicked (Game.addEntry { visible = formInputs.visible, hidden = formInputs.hidden } game)) ] [ Html.text "Avanti" ] ]
                ]

        ShowingCadaver game ->
            Html.text (Game.serialise game)


serialiseAsQueryParam : Game -> String
serialiseAsQueryParam game =
    BuildUrl.absolute [] [ BuildUrl.string "ongoing-state" (Game.serialise game) ]


players : List String -> List (Html.Html msg)
players otherPlayers =
    List.map Html.text otherPlayers
