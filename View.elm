module View exposing (view)

-- import Element.Events as Events

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (id)
import Types exposing (..)


view : Model -> Html Msg
view model =
    layout
        []
    <|
        column
            [ width fill
            , height fill
            ]
            [ mainContent model
            , menu model.streamingState model.filter
            ]


menuHeight : Int
menuHeight =
    60


mainContent : Model -> Element Msg
mainContent model =
    let
        h =
            case model.windowSize of
                Just size ->
                    px (size.height - menuHeight)

                Nothing ->
                    fill

        content =
            case model.page of
                FieldsPage ->
                    fieldsSelector model.fields model.settings.selectedField

                LogsPage ->
                    logLines model.lines

                SettingsPage ->
                    settingsPane model.settings

                TermsPage ->
                    termsSelector model.terms model.settings.selectedTerms
    in
    row
        [ htmlAttribute <| id "logs"
        , height h
        , scrollbarY
        ]
        [ column [] [ content ]
        ]


logLines : List LogLine -> Element msg
logLines lines =
    let
        line l =
            el [] <| text <| l.message
    in
    row
        [ Font.family
            [ Font.external
                { name = "Hack"
                , url = "http://cdn.jsdelivr.net/font-hack/2.020/css/hack-extended.min.css"
                }
            ]
        , Font.size 12
        ]
        [ column [ width fill ] <|
            List.map line lines
        ]


menu : State -> String -> Element Msg
menu streaming filter =
    let
        buttonText =
            case streaming of
                Active ->
                    "Pause"

                Paused ->
                    "Activate"
    in
    row
        [ width fill
        , height (px menuHeight)
        , Font.size 16
        , spacing 5
        , padding 5
        ]
        [ filterEntry filter
        , menuButton (StreamingState (newStreamingState streaming)) buttonText
        , menuButton ClearLogs "Clear"
        , menuButton ToggleTerms "Terms"
        , menuButton ToggleFields "Fields"
        , menuButton ToggleSettings "Settings"
        ]


fieldsSelector : List String -> String -> Element Msg
fieldsSelector fields selected =
    let
        field f =
            Input.checkbox []
                { onChange = Just (SelectField f)
                , checked = f == selected
                , icon = Nothing
                , label = Input.labelRight [] (text f)
                }
    in
    column [] <| List.map field fields


termsSelector : List String -> List String -> Element Msg
termsSelector fields selected =
    let
        field f =
            Input.checkbox []
                { onChange = Just (SelectTerm f)
                , checked = List.member f selected
                , icon = Nothing
                , label = Input.labelRight [] (text f)
                }
    in
    column [] <| List.map field fields


settingsPane : Settings -> Element Msg
settingsPane settings =
    let
        baseUrlEntry =
            Input.text []
                { onChange = Just (UpdateSettings << BaseUrl)
                , text = settings.esBaseUrl
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Elasticsearch base url")
                }

        indexPatternEntry =
            Input.text []
                { onChange = Just (UpdateSettings << IndexPattern)
                , text = settings.esIndexPattern
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Elasticsearch index pattern:")
                }

        maxResultsEntry =
            let
                maxResults =
                    case settings.esMaxResults of
                        Valid nmbr ->
                            toString nmbr

                        Invalid nmbrStr _ ->
                            nmbrStr
            in
            Input.text []
                { onChange = Just (UpdateSettings << MaxResults)
                , text = maxResults
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Elasticsearch return max results:")
                }

        maxLinesEntry =
            let
                maxLines =
                    case settings.maxLinesToKeep of
                        Valid nmbr ->
                            toString nmbr

                        Invalid nmbrStr _ ->
                            nmbrStr
            in
            Input.text []
                { onChange = Just (UpdateSettings << MaxLinesToKeep)
                , text = maxLines
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Max lines in buffer")
                }

        refreshEntry =
            Input.radio
                [ width fill
                , Background.color Color.white
                , Border.color Color.grey
                , Border.width 1
                , padding 15
                , spacing 15
                , pointer
                ]
                { selected = Just settings.refreshInterval
                , onChange = Just (UpdateSettings << SelectRefresh)
                , label =
                    Input.labelAbove
                        []
                        (text "Refresh Interval:")
                , options =
                    [ refreshOption One
                    , refreshOption Two
                    , refreshOption Five
                    , refreshOption Ten
                    , refreshOption Fifteen
                    , refreshOption Thirty
                    , refreshOption Sixty
                    ]
                }

        refreshOption interval =
            Input.option interval (text (intervalToString interval))
    in
    column
        [ padding 50
        , spacing 20
        ]
        [ baseUrlEntry
        , indexPatternEntry
        , maxResultsEntry
        , maxLinesEntry
        , refreshEntry
        ]


newStreamingState : State -> State
newStreamingState current =
    case current of
        Active ->
            Paused

        Paused ->
            Active


filterEntry : String -> Element Msg
filterEntry filter =
    Input.text
        [ Border.width 1
        , Border.rounded 4
        , Border.solid
        , Border.color Color.black
        ]
        { onChange = Just Filter
        , text = filter
        , placeholder = Nothing
        , label = Input.labelLeft [] (text "Filter:")
        }


menuButton : msg -> String -> Element msg
menuButton message label =
    Input.button
        [ Background.color Color.blue
        , Border.rounded 4
        , Border.solid
        , Border.width 1
        , Border.color Color.white
        , Font.color Color.white
        , paddingXY 20 5
        ]
        { onPress = Just message
        , label = text label
        }
