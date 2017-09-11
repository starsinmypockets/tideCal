port module Main exposing (..)

import Html exposing (Html, text, div, img, h1, h2, h3, p, button, ul, li, input, label, form, fieldset)
import Html.Attributes exposing (src, class, classList, id, for, type_, defaultValue, name, value)
import Html.Events exposing (onClick, onInput, onWithOptions, Options)
import Debug exposing (log)
import Json.Decode exposing (..)
import Tuple


---- MODEL ----


type alias Cal =
    { summary : String }


type alias Model =
    { client_id : String
    , discovery_docs : List String
    , scopes : String
    , messages : List ApiRes
    , calendars : List Cal
    , startDate : String
    , endDate : String
    , station : String
    , calName : String
    , units : String
    }


model : Model
model =
    { client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
    , discovery_docs = [ "https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest" ]
    , scopes = "https://www.googleapis.com/auth/calendar"
    , messages = []
    , calendars = []
    , startDate = ""
    , endDate = ""
    , station = ""
    , calName = ""
    , units = "English"
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
    | Submit
    | Station String
    | StartDate String
    | EndDate String
    | CalName String
    | Units String


doAuth : Msg -> Model -> ( Model, Cmd Msg )
doAuth msg model =
    ( model, Cmd.none )


doSignout : Msg -> Model -> ( Model, Cmd Msg )
doSignout msg model =
    ( model, Cmd.none )


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

        Submit ->
            let
                m =
                    model
                        |> log "MODEL"
            in
                ( model, Cmd.none )

        Station txt ->
            ( { model | station = txt }, Cmd.none )

        StartDate date ->
            ( { model | startDate = date }, Cmd.none )

        EndDate date ->
            ( { model | endDate = date }, Cmd.none )

        CalName name ->
            ( { model | calName = name }, Cmd.none )

        Units txt ->
            ( { model | units = txt }, Cmd.none )

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
    "Uses the https://tidesandcurrents.noaa.gov/api/ API to generate google calendar events and to import them into a new or existing calendar. ** DISCLAIMER ** This is a beta product - please use at your own discretion. To contribute or report an issue visit \n    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."


view : Model -> Html Msg
view model =
    div [ classList [ ( "row", True ), ( "container", True ) ] ]
        [ h1 [] [ text "Tides for Google Calendar" ]
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
            [ h2 [ for "station_id" ] [ text "Select Tide Information" ]
            , form [ class "row" ]
                [ label [ for "station_id", class "col-md-4" ] [ text "Station Id" ]
                , input [ id "station_id", class "col-md-4", onInput Station ] []
                , label [ for "start", class "col-md-4" ] [ text "Start Date" ]
                , input [ id "start", class "col-md-4", onInput StartDate ] []
                , label [ for "end", class "col-md-4" ] [ text "End Date" ]
                , input [ id "end", class "col-md-4", onInput EndDate ] []
                , label [ for "calendar_name", class "col-md-4" ] [ text "Enter calendar name for import" ]
                , input [ id "calendar_name ", class "col-md-4", onInput CalName ] []
                , fieldset [ class "row", onInput Units ]
                    [ p [ class "col-md-3" ] [ text "Units:" ]
                    , radio "English"
                    , radio "Metric"
                    , button [ onWithOptions "click" { preventDefault = True, stopPropagation = True } (Json.Decode.succeed Submit) ] [ text "Submit" ]
                    ]

                -- radio english / metric
                -- time zone default your time zone
                --
                ]
            ]
        , div [ class "row" ]
            [ h2 [] [ text "debug log" ]
            , ul [] (messageList model.messages)
            ]
        ]



-- template bits


radio : String -> Html msg
radio txt =
    label
        [ class "radios" ]
        [ input [ type_ "radio", name "units" ] []
        , text txt
        ]


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
