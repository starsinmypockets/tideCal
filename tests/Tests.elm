module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Main
import Dict
import Date
import DatePicker exposing (defaultSettings, DateEvent(..), DatePicker)


all : Test
all =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        model =
            { --
              client_id = "787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com"
            , signinStatus = False
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
            , showLog = "false"
            }
    in
        describe "A Test Suite"
            [ -- month type
              test "Month vals have leading zeros where appropriate" <|
                \_ ->
                    Expect.equal (Dict.get "Jan" Main.months) (Just "01")
            , test "Month vals do not have leading zeros where not appropriate" <|
                \_ ->
                    Expect.equal (Dict.get "Dec" Main.months) (Just "12")

            -- initial model
            , test "Initial model messages list is empty" <|
                \_ ->
                    Expect.equal model.messages []
            , test "Initial model calendars list is empty" <|
                \_ ->
                    Expect.equal model.calendars []
            , test "Initial model uiErrors list is empty" <|
                \_ ->
                    Expect.equal model.uiErrors []

            -- reset model
            -- form validation
            , test "Too small station id fails validation" <|
                \_ ->
                    Expect.equal (Main.validateStationId "100") True
            , test "Too large station id fails validation" <|
                \_ ->
                    Expect.equal (Main.validateStationId "100000000000000") True
            , test "Station id with letters fail validation" <|
                \_ ->
                    Expect.equal (Main.validateStationId "7A00000") True
            , test "Valid station id passes validation" <|
                \_ ->
                    Expect.equal (Main.validateStationId "7000000") False

            -- DATES
            --  noaaApiDateFromElmDate
            , test "Date formatter emits xxx's for invalid date" <|
                \_ ->
                    Expect.equal (Main.noaaApiDateFromElmDate Nothing) "XXXXXXX"
            , test "Date formatter handles valid dates as expected" <|
                \_ ->
                    let
                        d =
                            dateToMaybe "1/1/2010"
                    in
                        Expect.equal (Main.noaaApiDateFromElmDate d) "20100101"

            -- NOAA API
            , test "NOAA api request sane" <|
                \_ ->
                    let
                        baseUrl =
                            "https://tidesandcurrents.noaa.gov/api/datagetter?"

                        defaults =
                            "&product=predictions&format=json&interval=hilo&units=english&time_zone=lst&datum=MTL"

                        m =
                            { model | station = "7000000", startDate = (dateToMaybe "10102020"), endDate = (dateToMaybe "10102021") }

                        validUrl =
                            baseUrl ++ "station=" ++ m.station ++ "&begin_date=" ++ (Main.noaaApiDateFromElmDate m.startDate) ++ "&end_date=" ++ (Main.noaaApiDateFromElmDate m.endDate) ++ defaults
                    in
                        Expect.equal (Main.noaaUrl m) validUrl
            ]


dateToMaybe dateString =
    case Date.fromString dateString of
        Ok val ->
            Just val

        Err msg ->
            Nothing
