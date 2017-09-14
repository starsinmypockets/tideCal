port module Main exposing (..)

import Html exposing (Html, text, div, img, h1, h2, h3, p, button, ul, li, input, label, form, fieldset, a, span)
import Html.Attributes exposing (src, class, classList, id, for, type_, defaultValue, name, value, disabled, target, href)
import Html.Events exposing (onClick, onInput, onWithOptions, Options)
import Http
import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Tuple
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings, DateEvent(..), DatePicker)
import Validate exposing (..)


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
    { --
      client_id : String
    , discovery_docs : List String
    , scopes : String
    , messages : List ApiRes
    , calendars : List Cal
    , loading : Bool

    -- form data --
    , startDate : Maybe Date
    , endDate : Maybe Date
    , calName : String
    , units : String
    , station : String
    , uiErrors : List String

    --
    , error : Maybe String
    , noaaData : Maybe NOAAApiRes
    , calEventsJson : Maybe String
    , targetCalId : Maybe String
    , datePicker : DatePicker
    }


resetModel model =
    { model
        | startDate = Nothing
        , endDate = Nothing
        , calName = ""
        , units = ""
        , station = ""
        , uiErrors = []
    }


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        model =
            { --
              client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
            , discovery_docs = [ "https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest" ]
            , scopes = "https://www.googleapis.com/auth/calendar"
            , messages = []
            , calendars = []
            , loading = False

            -- form data
            , startDate = Nothing
            , endDate = Nothing
            , station = ""
            , calName = ""
            , units = "English"
            , uiErrors = []

            --
            , error = Nothing
            , noaaData = Nothing
            , targetCalId = Nothing
            , calEventsJson = Nothing
            , datePicker = datePicker
            }
    in
        ( model
        , Cmd.batch
            [ fromElm ( [], "init" )
            , Cmd.map DoDatePicker datePickerFx
            ]
        )


validateModel : Model -> List String
validateModel =
    Validate.all
        [ .calName >> ifBlank "Please enter a calendar name"
        , .station >> ifInvalid validateStationId "Please choose a valid station number (see info button next to station field)"
        ]


validateStationId val =
    let
        stationInt =
            String.toInt val
    in
        case (String.toInt val) of
            Ok num ->
                num < 1000000 || num > 9999999

            Err msg ->
                True



--        , .calName >> ifInvalid (\calName -> (List.string calName) < 6 ) "Please use a longer calendar name."
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
    | DoDatePicker DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send list ->
            ( model, fromElm list )

        Rcv res ->
            handleGApiRes res model

        Submit ->
            let
                uiErrors =
                    validateModel model
                        |> log "VALID"
            in
                if (List.length uiErrors > 0) then
                    ( { model
                        | messages = ( "Submit Err", "Form errors" ) :: model.messages
                        , uiErrors = uiErrors
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | messages = ( "Submit Ok", "Form Submit Success" ) :: model.messages
                        , uiErrors = []
                        , loading = True
                      }
                    , doNOOAReq model
                    )

        NOAARes (Ok data) ->
            let
                debug =
                    data
                        |> log "noaa ok"
            in
                handleNOAARes data model

        NOAARes (Err msg) ->
            ( { model | messages = (handleHttpError msg) :: model.messages }, Cmd.none )

        -- Form Interactions
        Station int ->
            ( { model | station = int }, Cmd.none )

        CalName name ->
            ( { model | calName = name }, Cmd.none )

        Units txt ->
            ( { model | units = txt }, Cmd.none )

        DoDatePicker msg ->
            toDatePicker msg model

        _ ->
            ( model, Cmd.none )


toDatePicker msg model =
    let
        ( newDatePicker, datePickerFx, event ) =
            DatePicker.update defaultSettings msg model.datePicker
    in
        ( { model
            | startDate =
                case event of
                    Changed date ->
                        date

                    _ ->
                        Nothing
            , datePicker = newDatePicker
          }
        , Cmd.map DoDatePicker datePickerFx
        )


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

            "addCalendar" ->
                let
                    calendarId =
                        Tuple.first res
                in
                    case (model.calEventsJson) of
                        Just json ->
                            ( { model | targetCalId = Just calendarId }, (fromElm ( [ calendarId, json ], "addEvents" )) )

                        Nothing ->
                            ( model, Cmd.none )

            "addEvents" ->
                ( (resetModel model), Cmd.none )

            "noGapi" ->
                ( { model | uiErrors = [ "We're sorry, the the Google Calendar service is not available. Try reloading the page." ] }, Cmd.none )

            "noGapiAuth" ->
                ( { model | uiErrors = [ "We're sorry, you need to authenticate with google. Press \"AUTHENTICATE\"!" ] }, Cmd.none )

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


noaaUrl model =
    "https://tidesandcurrents.noaa.gov/api/datagetter?"
        ++ "station="
        ++ model.station
        ++ "&begin_date=20180101&end_date=20180102&product=predictions&format=json&interval=hilo&units=english&time_zone=lst&datum=MTL"


doNOOAReq model =
    Http.send NOAARes <| Http.get (noaaUrl model) noaaDecoder


handleNOAARes : NOAAApiRes -> Model -> ( Model, Cmd Msg )
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
        ( { model
            | messages = ( "NOAA Response SUCCESS", "see console" ) :: model.messages
            , noaaData = Just data
            , calEventsJson = Just calEventsJson
          }
        , fromElm ( [ model.calName ], "addCalendar" )
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
    { description = (getTideTypeString tide) ++ " -- " ++ " " ++ (getTideDateString tide.date) }


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


getTideDateString : String -> String
getTideDateString date =
    let
        dd =
            String.split " " date

        day =
            case (List.head dd) of
                Just str ->
                    str

                Nothing ->
                    ""

        time =
            case (List.tail dd) of
                Just [ str ] ->
                    str

                _ ->
                    ""

        dayBits =
            String.split "-" day
    in
        case dayBits of
            [ a, b, c ] ->
                let
                    mo =
                        Result.withDefault 0 (String.toInt b) |> toString

                    day =
                        Result.withDefault 0 (String.toInt c) |> toString

                    year =
                        Result.withDefault 0 (String.toInt a) |> toString
                in
                    String.join "/" [ mo, day, year ] ++ " " ++ time

            _ ->
                date



---- VALIDATION ----
---- VIEW ----


introText : String
introText =
    "Uses the https://tidesandcurrents.noaa.gov/api/ API to generate google calendar events and to import them into a new or existing calendar. ** DISCLAIMER ** This is a beta product - please use at your own discretion. To contribute or report an issue visit \n    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."


view : Model -> Html Msg
view model =
    div [ classList [ ( "row", True ), ( "container", True ) ] ]
        [ h1 [] [ text "Tides for Google Calendar" ]
        , div [ classList [ ( "loading", model.loading ), ( "row", True ) ] ] []
        , p [] [ text introText ]
        , div [ class "row" ]
            [ h2 [] [ text "Your calendars" ]
            , ul [] (calendarList model.calendars)
            ]
        , div [ classList [ ( "row", True ), ( "error-box", True ) ] ] [ (errorBox model.uiErrors) ]
        , div [ classList [ ( "row", True ), ( "buttons", True ) ] ]
            [ button [ onClick (Send ( [], "auth" )) ] [ text "Authenticate" ]
            , button [ onClick (Send ( [], "signout" )) ] [ text "Sign Out" ]
            ]
        , div [ class "row" ]
            [ h2 [ for "station_id" ] [ text "Select Tide Information" ]
            , a [ target "blank", href "https://tidesandcurrents.noaa.gov/tide_predictions.html" ] [ text "Find tide station ID here" ]
            , form [ class "row" ]
                [ fieldset [ disabled (model.loading) ]
                    [ DatePicker.view model.startDate defaultSettings model.datePicker |> Html.map DoDatePicker ]
                , fieldset
                    [ disabled (model.loading) ]
                    [ label [ for "station_id", class "col-md-4" ] [ text "Station Id" ]
                    , label [] [ text "Test" ]
                    , input [ id "station_id", class "col-md-4", onInput Station, value model.station ] []
                    , label [ for "calendar_name", class "col-md-4" ] [ text "Enter calendar name for import" ]
                    , input [ id "calendar_name ", class "col-md-4", onInput CalName, value model.calName ] []
                    , fieldset [ class "row", onInput Units ]
                        [ p [ class "col-md-3" ] [ text "Units:" ]
                        , radio "English"
                        , radio "Metric"
                        , button [ onWithOptions "click" { preventDefault = True, stopPropagation = True } (JD.succeed Submit) ] [ text "Submit" ]
                        ]
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


errorBox uiErrors =
    div [ class "form-errors" ]
        [ ul [] (errorList uiErrors)
        ]


errorList uiErrors =
    List.map
        (\errMsg ->
            li [ class "ui-error" ] [ text errMsg ]
        )
        uiErrors



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
