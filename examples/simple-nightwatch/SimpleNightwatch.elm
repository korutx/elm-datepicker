module SimpleNightwatch exposing (main)

{-| This is a simple test suitable for automated browser testing (such as with nightwatch.js)
-}

import Browser
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), InputError(..), defaultSettings)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (id, type_)
import Html.Events exposing (onClick)
import Process
import Task
import Time exposing (Weekday(..))


type Msg
    = ToDatePicker DatePicker.Msg
    | NoOp


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , error : Maybe String
    }


settings : DatePicker.Settings
settings =
    { defaultSettings
        | isDisabled = \date -> modBy 2 (Date.toRataDie date) == 0
    }


init : ( Model, Cmd Msg )
init =
    let
        moonLandingDate =
            Date.fromCalendarDate 1969 Time.Jul 20
    in
    ( { date = Nothing
      , datePicker = DatePicker.initFromDate moonLandingDate
      , error = Nothing
      }
      -- trigger a NoOp command after two seconds. This is used to test
      -- that re-renders of the app do not cause things to dissapear.
    , delayedNoOpCmd { seconds = 2 }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker } as model) =
    case msg of
        ToDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update settings subMsg datePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            date

                error =
                    case dateEvent of
                        FailedInput (Invalid err) ->
                            Just <| "Parser error: " ++ err

                        FailedInput (Disabled d) ->
                            Just <| "Date disabled: " ++ Date.toIsoString d

                        FailedInput EmptyString ->
                            Just "Empty string error"

                        Picked _ ->
                            Nothing

                        None ->
                            model.error
            in
            ( { model
                | date = newDate
                , datePicker = newDatePicker
                , error = error
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view ({ date, datePicker, error } as model) =
    div []
        [ case date of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just d ->
                h1 [] [ text <| Date.format "MMM dd, yyyy" d ]
        , case error of
            Nothing ->
                text ""

            Just err ->
                h2 [ id "error" ] [ text err ]
        , DatePicker.view date settings datePicker
            |> Html.map ToDatePicker
        , div [ Html.Attributes.style "padding-top" "250px" ]
            [ button [ id "openpickerbtn", onClick <| ToDatePicker DatePicker.open, type_ "button" ] [ text "Open Datepicker" ]
            , button [ id "closepickerbtn", onClick <| ToDatePicker DatePicker.close, type_ "button" ] [ text "Close Datepicker" ]
            ]
        ]


delayedNoOpCmd : { seconds : Float } -> Cmd Msg
delayedNoOpCmd { seconds } =
    Process.sleep (seconds * 1000)
        |> Task.perform (\_ -> NoOp)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
