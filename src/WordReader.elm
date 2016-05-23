module WordReader exposing (..)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html


type alias Model =
    { words : List String
    , interval : Float
    , playing : Bool
    , wordList : Int
    }


type Msg
    = NoOp
    | UpdateInterval Float
    | StartWords Int
    | StopWords


initalModel : Model
initalModel =
    { words = [], interval = 0, playing = False, wordList = 100 }


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


view : Model -> Html Msg
view model =
    div [ class "center " ]
        [ wordButtons model
        , playButtons model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInterval time ->
            ( { model | interval = time }, Cmd.none )

        StartWords wordList ->
            ( { model | playing = True }, Cmd.none )

        StopWords ->
            ( { model | playing = False }, Cmd.none )


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
