module Types exposing (..)

import Http
import Window


type NestedKV a
    = Element a
    | Nested (List ( String, NestedKV a ))


type EsType
    = Text
    | Ip
    | Keyword
    | Date
    | Integer
    | Long
    | GeoPoint
    | HalfFloat
    | Double
    | Other


type Msg
    = ClearLogs
    | Filter String
    | GetLogs
    | NewFields (Result Http.Error (NestedKV EsType))
    | NewLogs (Result Http.Error ( Int, List ( LogLine, List String ) ))
    | NewTerms (Result Http.Error (List String))
    | NoOp
    | SelectField String Bool
    | SelectTerm String Bool
    | SelectTerms (List String)
    | UpdateSettings SettingsMsg
    | StreamingState State
    | ToggleFields
    | ToggleSettings
    | ToggleTerms
    | WindowSize Window.Size


type SettingsMsg
    = BaseUrl String
    | IndexPattern String
    | MaxResults String
    | MaxLinesToKeep String
    | SelectRefresh RefreshInterval


type State
    = Active
    | Paused


type SelectMenu
    = Open
    | Closed


type Page
    = LogsPage
    | FieldsPage
    | SettingsPage
    | TermsPage


type RefreshInterval
    = One
    | Two
    | Five
    | Ten
    | Fifteen
    | Thirty
    | Sixty


type alias Model =
    { esSearchAfter : List String
    , esSearchFrom : Maybe LogLine
    , fields : List String
    , filter : String
    , lines : List LogLine
    , page : Page
    , settings : Settings
    , streamingState : State
    , terms : List String
    , windowSize : Maybe Window.Size
    }


type alias Settings =
    { esBaseUrl : String
    , esIndexPattern : String
    , esMaxResults : IntEntry
    , maxLinesToKeep : IntEntry
    , refreshInterval : RefreshInterval
    , selectedField : String
    , selectedTerms : List String
    }


type IntEntry
    = Valid Int
    | Invalid String Int


type alias LogLine =
    { id : String
    , timestamp : String
    , message : String
    }


intervalToString : RefreshInterval -> String
intervalToString interval =
    case interval of
        One ->
            "1 sec"

        Two ->
            "2 sec"

        Five ->
            "5 sec"

        Ten ->
            "10 sec"

        Fifteen ->
            "15 sec"

        Thirty ->
            "30 sec"

        Sixty ->
            "1 m"


intervalToSeconds : RefreshInterval -> Float
intervalToSeconds interval =
    case interval of
        One ->
            1.0

        Two ->
            2.0

        Five ->
            5.0

        Ten ->
            10.0

        Fifteen ->
            15.0

        Thirty ->
            30.0

        Sixty ->
            60.0


secondsToRefreshInterval : Float -> RefreshInterval
secondsToRefreshInterval interval =
    case interval of
        1.0 ->
            One

        2.0 ->
            Two

        5.0 ->
            Five

        10.0 ->
            Ten

        15.0 ->
            Fifteen

        30.0 ->
            Thirty

        60.0 ->
            Sixty

        _ ->
            Five
