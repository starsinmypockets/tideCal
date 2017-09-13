port module Main exposing (..)

import Html exposing (Html, text, div, img, h1, h2, h3, p, button, ul, li, input, label, form, fieldset)
import Html.Attributes exposing (src, class, classList, id, for, type_, defaultValue, name, value)
import Html.Events exposing (onClick, onInput, onWithOptions, Options)
import Http
import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Tuple
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)


-- import DatePicker exposing (defaultSettings)
---- MODEL ----


type alias StationData =
    { id : String, name : String, lat : String, lon : String }


type alias TideList =
    List Tide


type alias Tide =
    { date : String
    , tideType : String
    }


type alias NOAAApiRes =
    { predictions : TideList }


type alias Cal =
    { summary : String }


type alias CalEvent =
    { description : String }


type alias CalEventList =
    List CalEvent


type alias Model =
    { client_id : String
    , discovery_docs : List String
    , scopes : String
    , messages : List ApiRes
    , calendars : List Cal
    , startDate : Maybe String
    , endDate : Maybe String
    , calName : Maybe String
    , units : Maybe String
    , error : Maybe String
    , noaaData : Maybe NOAAApiRes
    , station : Maybe String
    }


model : Model
model =
    { client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
    , discovery_docs = [ "https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest" ]
    , scopes = "https://www.googleapis.com/auth/calendar"
    , messages = []
    , calendars = []
    , startDate = Nothing
    , endDate = Nothing
    , station = Nothing
    , calName = Nothing
    , units = Just "English"
    , error = Nothing
    , noaaData = Nothing
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
    = Signout
    | Send ( List String, String )
    | Rcv ( GapiRes, ApiCmd )
    | NoOp
    | Submit
    | Station String
    | StartDate String
    | EndDate String
    | CalName String
    | Units String
    | NOAARes (Result Http.Error NOAAApiRes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send list ->
            ( model, fromElm list )

        Rcv res ->
            handleGApiRes res model

        Submit ->
            let
                valid =
                    (validateDates >> validateStation >> validateCalName) model
            in
                case valid of
                    Err msg ->
                        ( { model | messages = ( "Submit Err", msg ) :: model.messages }, Cmd.none )

                    Ok msg ->
                        ( { model | messages = ( "Submit Ok", msg ) :: model.messages }, doNOOAReq )

        NOAARes (Ok data) ->
            let
                debug =
                    data
                        |> log "noaa ok"
            in
                handleNOAARes data model

        NOAARes (Err msg) ->
            ( { model | messages = (handleHttpError msg) :: model.messages }, Cmd.none )

        Station txt ->
            ( { model | station = Just txt }, Cmd.none )

        StartDate date ->
            ( { model | startDate = Just date }, Cmd.none )

        EndDate date ->
            ( { model | endDate = Just date }, Cmd.none )

        CalName name ->
            ( { model | calName = Just name }, Cmd.none )

        Units txt ->
            ( { model | units = Just txt }, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleGApiRes : ApiRes -> Model -> ( Model, Cmd Msg )
handleGApiRes res model_ =
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
                        JD.map Cal (JD.field "summary" JD.string)

                    calListDecoder =
                        JD.list calDecoder

                    cals =
                        JD.decodeString calListDecoder payload
                in
                    case cals of
                        Ok vals ->
                            ( { model | calendars = vals }, Cmd.none )

                        Err msg ->
                            ( model, Cmd.none )

            -- noop
            _ ->
                ( model, Cmd.none )



--- NOAA API STUFF ---
-- @@TODO - figure out how to decode the api response from NOAA


tideDecoder : Decoder Tide
tideDecoder =
    JD.map2
        Tide
        (JD.field "t" JD.string)
        (JD.field "type" JD.string)


tideListDecoder : Decoder TideList
tideListDecoder =
    (JD.list tideDecoder)


noaaDecoder : Decoder NOAAApiRes
noaaDecoder =
    JD.map
        NOAAApiRes
        (JD.field "predictions" tideListDecoder)


noaaUrl =
    "https://tidesandcurrents.noaa.gov/api/datagetter?station=9414290&begin_date=20180101&end_date=20180102&product=predictions&format=json&interval=hilo&units=english&time_zone=lst&datum=MTL"


doNOOAReq =
    Http.send NOAARes <| Http.get noaaUrl noaaDecoder


handleNOAARes data model =
    let
        d =
            data
                |> log "noaa data"

        calEvents =
            createCalendarEvents data

        calEventsJson =
            encodeCalendarEvents calEvents
                |> log "events json"
    in
        -- generate calendar data
        -- insert new calendar with events
        --      create calendar
        --      add events
        -- catch that in update
        --      confirm success
        --done
        ( { model
            | messages = ( "NOAA Response SUCCESS", "see console" ) :: model.messages
            , noaaData = Just data
          }
        , fromElm ( [ "testCal1" ], "addCalendar" )
          -- get validated calname from model
        )


handleHttpError msg =
    case msg of
        Http.BadUrl response ->
            ( "Bad Url", response )

        Http.NetworkError ->
            ( "No Network", "" )

        Http.Timeout ->
            ( "Network Timeout", "" )

        Http.BadStatus response ->
            ( "API Failure", (toString response) )

        Http.BadPayload response _ ->
            ( "Bad Payload", (toString response) )


encodeCalEvent : CalEvent -> JE.Value
encodeCalEvent event =
    JE.object
        [ ( "description", JE.string event.description ) ]


encodeCalendarEvents : CalEventList -> String
encodeCalendarEvents eventList =
    JE.encode 2 (JE.list (List.map encodeCalEvent eventList))


createCalendarEvents : NOAAApiRes -> CalEventList
createCalendarEvents data =
    let
        tides =
            data.predictions
    in
        List.map (tideToCalEvent) tides
            |> log "return tides"


tideToCalEvent : Tide -> CalEvent
tideToCalEvent tide =
    { description = (getTideTypeString tide) ++ " -- " ++ " " ++ tide.date }


getTideTypeString : Tide -> String
getTideTypeString tide =
    let
        tt =
            String.trim tide.tideType
                |> log "TT"
    in
        if tt == "H" || tt == "HH" then
            "High Tide"
        else if tt == "L" || tt == "LL" then
            "Low Tide"
        else
            "Unknown Tide"



---- VALIDATION ----


validateDates model =
    Ok "Ok"


validateStation model =
    Ok "Ok"


validateCalName model =
    Ok "Ok"



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
                    , button [ onWithOptions "click" { preventDefault = True, stopPropagation = True } (JD.succeed Submit) ] [ text "Submit" ]
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
