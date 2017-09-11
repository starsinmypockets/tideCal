port module Main exposing (..)

import Html exposing (Html, text, div, img, h1, h2, h3, p, button, ul, li)
import Html.Attributes exposing (src, class, classList)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Json.Decode exposing (..)
import Tuple


---- MODEL ----


type alias Cal =
    { summary : String }


type alias Model =
    { client_id : String
    , count : Int
    , discovery_docs : List String
    , scopes : String
    , messages : List ApiRes
    , calendars : List Cal
    }


model : Model
model =
    { client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
    , discovery_docs = [ "https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest" ]
    , scopes = "https://www.googleapis.com/auth/calendar"
    , count = 0
    , messages = []
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


handleApiRes : ApiRes -> Model -> ( Model, Cmd Msg )
handleApiRes res model_ =
    let
        -- log api call
        model =
            { model_ | messages = res :: model_.messages }

        payload =
            Tuple.first res

        msg =
            Tuple.second res
    in
        case msg of
            "init" ->
                ( model, Cmd.none )

            "auth" ->
                ( model, fromElm ( [], "getCalendars" ) )

            "signout" ->
                ( { model | calendars = [] }, Cmd.none )

            "getCalendars" ->
                let
                    calDecoder =
                        map Cal (field "summary" string)

                    calListDecoder =
                        list calDecoder

                    cals =
                        decodeString calListDecoder payload
                in
                    case cals of
                        Ok vals ->
                            ( { model | calendars = vals }, Cmd.none )

                        Err msg ->
                            ( model, Cmd.none )

            -- noop
            _ ->
                ( model, Cmd.none )



---- VIEW ----


introText : String
introText =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."


view : Model -> Html Msg
view model =
    div [ classList [ ( "row", True ), ( "container", True ) ] ]
        [ h1 [] [ text " Tides for Google Calendar " ]
        , p [] [ text introText ]
        , div [ class "row" ]
            [ h2 [] [ text "Your calendars" ]
            , ul [] (calendarList model.calendars)
            ]
        , div [ classList [ ( "row", True ), ( "buttons", True ) ] ]
            [ button [ onClick (Send ( [], "auth" )) ] [ text "Authenticate" ]
            , button [ onClick (Send ( [], "signout" )) ] [ text "Sign Out" ]
            , button [ onClick (Send ( [], "init" )) ] [ text "INIT" ]
            ]
        , div [ class "row" ]
            [ h2 [] [ text "debug log" ]
            , ul [] (messageList model.messages)
            ]
        ]



-- template bits


messageList : List ApiRes -> List (Html msg)
messageList msgs =
    List.foldr
        (\res acc ->
            let
                markup =
                    li [] [ text (Tuple.first res), text "  --  ", text (Tuple.second res) ]
            in
                (::)
                    markup
                    acc
        )
        []
        msgs


calendarList : List Cal -> List (Html msg)
calendarList cals =
    List.foldr
        (\cal acc ->
            li [] [ text cal.summary ]
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
