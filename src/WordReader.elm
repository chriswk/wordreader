module WordReader exposing (..)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import Array exposing (Array)
import Random exposing (generate, Generator)
import Random.Array exposing (sample)
import Time exposing (millisecond, Time)


type alias Model =
    { words : Array String
    , interval : Float
    , playing : Bool
    , wordList : Int
    , currentWord : Maybe String
    }


type Msg
    = NoOp
    | UpdateInterval Float
    | Tick Float
    | StartWords Int
    | SelectedWord (Maybe String)
    | StopWords


initalModel : Model
initalModel =
    { words = Array.fromList [ "Hello", "Goodbye" ], interval = 0, playing = False, wordList = 100, currentWord = Nothing }


wordButtons : Model -> Html Msg
wordButtons model =
    div [ style [ ( "float", "left" ) ] ]
        [ button [ class "b", onClick (StartWords 100) ] [ text "100 ord" ]
        , button [ class "b", onClick (StartWords 200) ] [ text "200 ord" ]
        , button [ class "b", onClick (StartWords 300) ] [ text "300 ord" ]
        ]


playButtons : Model -> Html Msg
playButtons model =
    div [ style [ ( "float", "right" ) ] ]
        [ button [ class "b", onClick (UpdateInterval 500) ] [ text "Blink" ]
        , button [ class "b", onClick (UpdateInterval 1000) ] [ text "1 sek" ]
        , button [ class "b", onClick (UpdateInterval 2000) ] [ text "2 sek" ]
        ]


wordBox : Model -> Html Msg
wordBox model =
    div [ style [ ( "font-size", "15vmax" ) ] ]
        [ text
            (case model.currentWord of
                Just w ->
                    w

                Nothing ->
                    ""
            )
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "center " ]
            [ wordButtons model
            , playButtons model
            ]
        , div [ class "center" ]
            [ wordBox model
            ]
        ]


selectNewWord model =
    sample model.words


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (model.interval * millisecond) Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInterval time ->
            ( { model | interval = time }, Cmd.none )

        StartWords wordList ->
            ( { model | playing = True }, Random.generate SelectedWord (selectNewWord model) )

        SelectedWord word ->
            ( { model | currentWord = word }, Cmd.none )

        StopWords ->
            ( { model | playing = False }, Cmd.none )

        Tick time ->
            ( model, Random.generate SelectedWord (selectNewWord model) )


init : ( Model, Cmd Msg )
init =
    ( initalModel, Cmd.none )


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
