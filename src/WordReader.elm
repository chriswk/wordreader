module WordReader exposing (..)

import Html
import Html exposing (..)
import Http exposing (getString)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import Array exposing (Array, fromList)
import Random exposing (generate, Generator)
import Random.Array exposing (sample)
import Time exposing (millisecond, Time)
import Task exposing (Task)
import Json.Decode exposing (list, string)
import String as String


type alias Model =
    { words : Array String
    , interval : Float
    , playing : Bool
    , wordCount : Int
    , currentWord : Maybe String
    , lastMessage : String
    }


type Msg
    = NoOp
    | UpdateInterval Float
    | Tick Float
    | StartWords
    | SelectedWord (Maybe String)
    | FetchSucceed (Array String)
    | FetchFail Http.Error
    | StopWords


initalModel : Model
initalModel =
    { words = Array.fromList [ "Hello", "Goodbye" ], interval = 500, playing = False, wordCount = 0, currentWord = Nothing, lastMessage = "" }


wordButtons : Model -> Html Msg
wordButtons model =
    div [ style [ ( "float", "left" ) ] ]
        [ button [ class "b", onClick StartWords ] [ text "Start!" ]
        , button [ class "b", onClick StopWords ] [ text "Ta en pause!" ]
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
        , div [ class "center" ]
            [ text model.lastMessage
            , text (toString model.wordCount)
            ]
        ]


fetchWords : Task Http.Error String
fetchWords =
    Http.getString "http://chriswk.github.io/wordreader/data/ord1000.txt"


selectNewWord model =
    sample model.words


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every (model.interval * millisecond) Tick
    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInterval time ->
            ( { model | interval = time, playing = True }, Cmd.none )

        StartWords ->
            ( { model | playing = True }, Random.generate SelectedWord (selectNewWord model) )

        SelectedWord word ->
            ( { model | currentWord = word }, Cmd.none )

        StopWords ->
            ( { model | playing = False }, Cmd.none )

        Tick time ->
            ( model, Random.generate SelectedWord (selectNewWord model) )

        FetchSucceed wordList ->
            ( { model | words = wordList, wordCount = Array.length wordList }, Cmd.none )

        FetchFail e ->
            ( { model | lastMessage = toString e }, Cmd.none )


getWords : Cmd Msg
getWords =
    let
        url =
            "http://chriswk.github.io/wordreader/data/ord1000.txt"

        wordList =
            Http.getString url

        splitOnNewLine =
            Task.map (\s -> String.split "\n" s) wordList

        toArray =
            Task.map fromList splitOnNewLine
    in
        Task.perform FetchFail FetchSucceed toArray


init : ( Model, Cmd Msg )
init =
    ( initalModel, getWords )


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
