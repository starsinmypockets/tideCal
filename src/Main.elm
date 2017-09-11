port module Main exposing (..)

import Html exposing (Html, text, div, img, h1, h2, h3, p, button, ul)
import Html.Attributes exposing (src, class, classList)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Tuple


---- MODEL ----


type alias Model =
    { client_id : String
    , count : Int
    , discovery_docs : List String
    , scopes : String
    , messages : List ApiRes
    , calendars : List String
    }


model : Model
model =
    { client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
    , discovery_docs = [ "https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest" ]
    , scopes = "https://www.googleapis.com/auth/calendar"
    , count = 0
    , messages = [ ( "", "" ) ]
    , calendars = []
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



---- UPDATE ----


port fromElm : ( List String, String ) -> Cmd msg


type alias GapiRes =
    String


type alias ApiCmd =
    String


type alias ApiRes =
    ( GapiRes, ApiCmd )


type Msg
    = Auth
    | Signout
    | Send ( List String, String )
    | Rcv ( GapiRes, ApiCmd )
    | NoOp


doAuth : Msg -> Model -> ( Model, Cmd Msg )
doAuth msg model =
    ( { model | count = model.count + 10 }, Cmd.none )


doSignout : Msg -> Model -> ( Model, Cmd Msg )
doSignout msg model =
    ( { model | count = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Auth ->
            doAuth msg model

        Signout ->
            doSignout msg model

        Send list ->
            ( model, fromElm list )

        Rcv res ->
            handleApiRes res model

        _ ->
            ( model, Cmd.none )



---- VIEW ----


introText : String
introText =
    "Description hercome_"


view : Model -> Html Msg
view model =
    div [ classList [ ( "row", True ), ( "container", True ) ] ]
        [ h1 [] [ text " Tides for Google Calendar " ]
        , p [] [ text introText ]
        , ul [ class "row" ] (messageList model.messages)
        , div [ class "row" ] (calendarList model.calendars)
        , div [ classList [ ( "row", True ), ( "buttons", True ) ] ]
            [ button [ onClick (Send ( [], "auth" )) ] [ text "Authenticate" ]
            , button [ onClick (Send ( [], "signout" )) ] [ text "Sign Out" ]
            , button [ onClick (Send ( [], "init" )) ] [ text "INIT" ]
            ]
        ]



-- template bits


messageList : List ApiRes -> List (Html msg)
messageList msgs =
    List.foldr
        (\res acc ->
            let
                markup =
                    div [ class "row" ]
                        [ p [ class "col-md-4" ] [ text (Tuple.first res) ]
                        , p [ class "col-md-4" ] [ text (Tuple.second res) ]
                        ]
            in
                (::)
                    markup
                    acc
        )
        []
        msgs


calendarList : List String -> List (Html msg)
calendarList cals =
    List.foldr
        (\cal acc ->
            p [ class "col-md-4" ] [ text cal ]
                :: acc
        )
        []
        cals



---- PROGRAM ----


port fromJs : (ApiRes -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fromJs Rcv


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
