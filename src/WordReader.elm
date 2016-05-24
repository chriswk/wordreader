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
import String as String


type alias Model =
    { words : List String
    , interval : Float
    , playing : Bool
    , wordCount : Maybe Int
    , currentWord : Maybe String
    , lastMessage : String
    , wordsShown : Int
    }


type Msg
    = NoOp
    | UpdateInterval Float
    | Tick Float
    | StartWords (Maybe Int)
    | SelectedWord (Maybe String)
    | FetchSucceed (List String)
    | FetchFail Http.Error
    | StopWords


initalModel : Model
initalModel =
    { words = [], interval = 500, playing = False, wordCount = Nothing, currentWord = Nothing, lastMessage = "", wordsShown = 0 }


wordButtons : Model -> Html Msg
wordButtons model =
    div [ style [ ( "float", "left" ) ] ]
        [ button [ class "b", onClick (StartWords Nothing) ] [ text "1000 mest brukte ord" ]
        , button [ class "b", onClick (StartWords (Just 100)) ] [ text "100 mest brukte ord" ]
        , button [ class "b", onClick (StartWords (Just 200)) ] [ text "100-200 mest brukte ord" ]
        , button [ class "b", onClick (StartWords (Just 300)) ] [ text "200-300" ]
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


counter : Int -> Html Msg
counter count =
    div [ class "wordcount" ] [ text (toString count) ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "center " ]
            [ wordButtons model
            , counter model.wordsShown
            , playButtons model
            ]
        , div [ class "center" ]
            [ wordBox model
            ]
        , div [ class "center" ]
            [ text model.lastMessage
            ]
        ]


selectNewWord : Model -> Generator (Maybe String)
selectNewWord model =
    let
        wordCount =
            model.wordCount

        wordList =
            case wordCount of
                Just c ->
                    if c > 100 then
                        List.take 100 (List.drop (c - 100) model.words)
                    else
                        List.take 100 model.words

                Nothing ->
                    model.words

        wordArray =
            fromList wordList
    in
        sample wordArray


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

        StartWords count ->
            ( { model | playing = True, wordCount = count }, Random.generate SelectedWord (selectNewWord model) )

        SelectedWord word ->
            ( { model | currentWord = word, wordsShown = model.wordsShown + 1 }, Cmd.none )

        StopWords ->
            ( { model | playing = False }, Cmd.none )

        Tick _ ->
            ( model, Random.generate SelectedWord (selectNewWord model) )

        FetchSucceed wordList ->
            ( { model | words = wordList }, Cmd.none )

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
    in
        Task.perform FetchFail FetchSucceed splitOnNewLine


init : ( Model, Cmd Msg )
init =
    ( initalModel, getWords )


main : Program Never
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
