module Main exposing (main)

import Dom.Scroll exposing (toBottom)
import Elastic
import Html
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import Task
import Time
import Types exposing (..)
import View exposing (view)
import Window


-- Init


init : Maybe String -> ( Model, Cmd Msg )
init storedVal =
    let
        settings =
            sessionFromJson storedVal
    in
    ( { esSearchAfter = []
      , esSearchFrom = Nothing
      , fields = []
      , filter = "*"
      , lines = []
      , page = LogsPage
      , settings = settings
      , streamingState = Paused
      , terms = []
      , windowSize = Nothing
      }
    , Task.perform WindowSize Window.size
    )



-- Session storage


defaultMaxLinesToKeep : Int
defaultMaxLinesToKeep =
    10000


defaultMaxResults : Int
defaultMaxResults =
    100


defaultSettings : Settings
defaultSettings =
    { esBaseUrl = "http://localhost:9200"
    , esIndexPattern = "filebeat-*"
    , esMaxResults = Valid defaultMaxResults
    , maxLinesToKeep = Valid defaultMaxLinesToKeep
    , refreshInterval = Five
    , selectedField = ""
    , selectedTerms = []
    }


sessionFromJson : Maybe String -> Settings
sessionFromJson json =
    let
        decodeSettings =
            Decode.map7 Settings
                (Decode.field "esBaseUrl" Decode.string)
                (Decode.field "esIndexPattern" Decode.string)
                (Decode.map Valid (Decode.field "esMaxResults" Decode.int))
                (Decode.map Valid (Decode.field "maxLinesToKeep" Decode.int))
                (Decode.map secondsToRefreshInterval (Decode.field "refreshInterval" Decode.float))
                (Decode.field "selectedField" Decode.string)
                (Decode.field "selectedTerms" (Decode.list Decode.string))

        result =
            case json of
                Just j ->
                    Decode.decodeString (Decode.field "settings" decodeSettings) j

                Nothing ->
                    Ok defaultSettings
    in
    case result of
        Ok val ->
            val

        Err err ->
            Debug.log err defaultSettings


sessionToJson : Settings -> String
sessionToJson settings =
    let
        maxResults =
            case settings.esMaxResults of
                Valid nmbr ->
                    nmbr

                Invalid _ df ->
                    df
    in
    Encode.object
        [ ( "settings"
          , Encode.object
                [ ( "esBaseUrl", Encode.string settings.esBaseUrl )
                , ( "esIndexPattern", Encode.string settings.esIndexPattern )
                , ( "esMaxResults", Encode.int maxResults )
                , ( "refreshInterval", Encode.float (intervalToSeconds settings.refreshInterval) )
                , ( "selectedField", Encode.string settings.selectedField )
                , ( "selectedTerms", Encode.list (List.map Encode.string settings.selectedTerms) )
                ]
          )
        ]
        |> Encode.encode 0



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        Filter filter ->
            let
                newModel =
                    { model | filter = filter }
            in
            ( newModel, Cmd.none )

        ClearLogs ->
            ( { model | lines = [] }, Cmd.none )

        GetLogs ->
            ( model, Elastic.requestLogs model )

        NewFields result ->
            let
                fields =
                    case result of
                        Ok vals ->
                            vals
                                |> Elastic.fieldNames
                                |> List.sort

                        Err err ->
                            Debug.log (toString err) []
            in
            ( { model
                | fields = fields
                , page = FieldsPage
              }
            , Cmd.none
            )

        NewTerms result ->
            let
                newTerms =
                    case result of
                        Ok terms ->
                            terms
                                |> List.sort

                        Err err ->
                            Debug.log (toString err) []
            in
            ( { model
                | terms = newTerms
                , page = TermsPage
              }
            , Cmd.none
            )

        NewLogs result ->
            let
                ( newLines, esSearchAfter, esSearchFrom ) =
                    case result of
                        Ok ( total, linesAndSearchAfter ) ->
                            let
                                lines =
                                    linesAndSearchAfter
                                        |> List.map Tuple.first
                                        |> List.reverse

                                maxResults =
                                    case model.settings.esMaxResults of
                                        Valid nmbr ->
                                            nmbr

                                        Invalid _ df ->
                                            df

                                linesWithSkip =
                                    if total > maxResults then
                                        LogLine "" "" (" ----- skipped: " ++ (toString <| total - maxResults) ++ " -----") :: lines
                                    else
                                        lines

                                filtered =
                                    case model.esSearchFrom of
                                        Just f ->
                                            if List.member f lines then
                                                lines
                                                    |> dropWhileNotFound f
                                            else
                                                linesWithSkip

                                        Nothing ->
                                            linesWithSkip

                                ( newestLine, searchAfter ) =
                                    linesAndSearchAfter
                                        |> List.head
                                        |> Maybe.map (\( a, b ) -> ( Just a, b ))
                                        |> Maybe.withDefault ( Nothing, [] )
                            in
                            ( filtered
                            , searchAfter
                            , newestLine
                            )

                        Err err ->
                            Debug.log (toString err) ( model.lines, model.esSearchAfter, model.esSearchFrom )

                updatedNumLines =
                    List.length model.lines + List.length newLines

                maxLinesReached =
                    case model.settings.maxLinesToKeep of
                        Valid nmbr ->
                            updatedNumLines > nmbr

                        Invalid _ lastnmbr ->
                            updatedNumLines > lastnmbr

                capLines =
                    if maxLinesReached then
                        model.lines ++ [ LogLine "" "" " ----- max lines reached - hit 'Clear' to continue -----" ]
                    else
                        model.lines ++ newLines
            in
            ( { model
                | lines = capLines
                , esSearchAfter = esSearchAfter
                , esSearchFrom = esSearchFrom
                , streamingState =
                    if maxLinesReached then
                        Paused
                    else
                        model.streamingState
              }
            , if model.page == LogsPage then
                scroll "logs"
              else
                Cmd.none
            )

        SelectField field selected ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    if selected then
                        { oldSettings | selectedField = field }
                    else
                        { oldSettings
                            | selectedField = ""
                            , selectedTerms = []
                        }
            in
            ( { model | settings = newSettings }
            , Ports.storeSession (sessionToJson newSettings)
            )

        SelectTerm term selected ->
            let
                oldSettings =
                    model.settings

                filteredTerm =
                    if selected then
                        term :: model.settings.selectedTerms
                    else
                        List.filter (\x -> x /= term) model.settings.selectedTerms

                newSettings =
                    { oldSettings
                        | selectedTerms = filteredTerm
                    }
            in
            ( { model | settings = newSettings }
            , Ports.storeSession (sessionToJson newSettings)
            )

        SelectTerms terms ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings
                        | selectedTerms = terms
                    }
            in
            ( { model
                | page = LogsPage
                , settings = newSettings
              }
            , Ports.storeSession (sessionToJson newSettings)
            )

        StreamingState streamingState ->
            ( { model | streamingState = streamingState }
            , if streamingState == Active then
                Elastic.requestLogs model
              else
                Cmd.none
            )

        ToggleSettings ->
            case model.page of
                SettingsPage ->
                    ( { model | page = LogsPage }, Cmd.none )

                _ ->
                    ( { model | page = SettingsPage }, Cmd.none )

        ToggleTerms ->
            case model.page of
                TermsPage ->
                    ( { model | page = LogsPage }, Cmd.none )

                _ ->
                    ( model, Elastic.requestTerms model )

        ToggleFields ->
            case model.page of
                FieldsPage ->
                    ( { model | page = LogsPage }, Cmd.none )

                _ ->
                    ( model, Elastic.requestFields model )

        UpdateSettings settingsMsg ->
            let
                newSettings =
                    updateSettings settingsMsg model.settings
            in
            ( { model | settings = newSettings }
            , Ports.storeSession (sessionToJson newSettings)
            )

        WindowSize size ->
            ( { model | windowSize = Just size }, Cmd.none )


dropWhileNotFound : a -> List a -> List a
dropWhileNotFound entry list =
    case list of
        [] ->
            []

        x :: xs ->
            if x == entry then
                xs
            else
                dropWhileNotFound entry xs


updateSettings : SettingsMsg -> Settings -> Settings
updateSettings settingsMsg settings =
    case settingsMsg of
        BaseUrl url ->
            { settings | esBaseUrl = url }

        IndexPattern pattern ->
            { settings | esIndexPattern = pattern }

        MaxResults nmbrStr ->
            let
                maxResult =
                    case String.toInt nmbrStr of
                        Ok nmbr ->
                            Valid nmbr

                        Err _ ->
                            Invalid nmbrStr defaultMaxResults
            in
            { settings | esMaxResults = maxResult }

        MaxLinesToKeep nmbrStr ->
            let
                maxLines =
                    case String.toInt nmbrStr of
                        Ok nmbr ->
                            Valid nmbr

                        Err _ ->
                            Invalid nmbrStr defaultMaxLinesToKeep
            in
            { settings | maxLinesToKeep = maxLines }

        SelectRefresh interval ->
            { settings
                | refreshInterval = interval
            }


scroll : String -> Cmd Msg
scroll node_id =
    Task.attempt (always NoOp) <| toBottom node_id



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        streaming =
            case model.streamingState of
                Paused ->
                    Sub.none

                Active ->
                    Time.every (intervalToSeconds model.settings.refreshInterval * Time.second) (\_ -> GetLogs)
    in
    Sub.batch
        [ Window.resizes WindowSize
        , streaming
        ]



-- Main


main : Program (Maybe String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
