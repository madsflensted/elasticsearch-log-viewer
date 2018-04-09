module Elastic exposing (fieldNames, requestFields, requestLogs, requestTerms)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Types exposing (..)


requestLogs : Model -> Cmd Msg
requestLogs model =
    let
        queryObj =
            encodeAt [ "query_string", "query" ] (Encode.string model.filter)

        termList =
            let
                termEntry t =
                    encodeAt [ "term", model.settings.selectedField ] (Encode.string t)
            in
            List.map termEntry model.settings.selectedTerms

        termObj =
            encodeAt [ "bool", "should" ] (Encode.list termList)

        dateRangeObj =
            let
                ts =
                    case model.esSearchFrom of
                        Just l ->
                            l.timestamp

                        Nothing ->
                            "now-1d/d"
            in
            encodeAt [ "range", "@timestamp" ] <| Encode.object [ ( "gte", Encode.string ts ) ]

        searchAfterObj =
            [ ( "search_after", Encode.list (List.map Encode.string model.esSearchAfter) ) ]

        body =
            let
                maxResults =
                    case model.settings.esMaxResults of
                        Valid mx ->
                            mx

                        Invalid _ df ->
                            df
            in
            Encode.object
                [ ( "size", Encode.int maxResults )
                , ( "query"
                  , Encode.object
                        [ ( "bool"
                          , Encode.object
                                [ ( "must"
                                  , Encode.list
                                        [ queryObj
                                        , termObj
                                        , dateRangeObj
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                , ( "sort"
                  , Encode.list
                        [ Encode.object
                            [ ( "@timestamp", Encode.string "desc" )
                            , ( "_uid", Encode.string "desc" )
                            ]
                        ]
                  )
                ]

        url =
            esSearchUrl model.settings.esBaseUrl model.settings.esIndexPattern

        req =
            request "POST" url (Http.jsonBody body) decodeSearch
    in
    Http.send NewLogs req


encodeAt : List String -> Value -> Value
encodeAt keys value =
    let
        enc key val =
            Encode.object [ ( key, val ) ]
    in
    List.foldr enc value keys


esSearchUrl : String -> String -> String
esSearchUrl base index =
    base ++ "/" ++ index ++ "/_search"


esMappingUrl : String -> String -> String
esMappingUrl base index =
    base ++ "/" ++ index ++ "/_mapping"


requestTerms : Model -> Cmd Msg
requestTerms model =
    let
        body =
            encodeAt [ "aggs", "genres", "terms" ]
                (Encode.object
                    [ ( "field", Encode.string model.settings.selectedField )
                    , ( "size", Encode.int 50 )
                    , ( "order", Encode.object [ ( "_count", Encode.string "desc" ) ] )
                    ]
                )

        url =
            esSearchUrl model.settings.esBaseUrl model.settings.esIndexPattern

        req =
            request "POST" url (Http.jsonBody body) decodeTerms
    in
    if model.settings.selectedField /= "" then
        Http.send NewTerms req
    else
        Cmd.none


requestFields : Model -> Cmd Msg
requestFields model =
    let
        url =
            esMappingUrl model.settings.esBaseUrl model.settings.esIndexPattern

        req =
            request "GET" url Http.emptyBody decodeIndices
    in
    Http.send NewFields req


request : String -> String -> Http.Body -> Decoder a -> Http.Request a
request method url body decoder =
    Http.request
        { method = method
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


decodeSearch : Decoder ( Int, List ( LogLine, List String ) )
decodeSearch =
    let
        decodeResult =
            Decode.map2 (,) decodeTotal decodeHits

        decodeTotal =
            Decode.field "total" Decode.int

        decodeHits =
            Decode.field "hits" decodeMessages

        decodeMessages =
            Decode.list decodeHit

        decodeHit =
            Decode.map2 (,) decodeLogLine decodeSort

        decodeSort =
            Decode.field "sort" decodeSortFields

        decodeSortFields =
            Decode.list <| Decode.oneOf [ Decode.map toString Decode.int, Decode.string ]

        decodeLogLine =
            Decode.map3 LogLine (Decode.field "_id" Decode.string) (Decode.at [ "_source", "@timestamp" ] Decode.string) (Decode.at [ "_source", "message" ] Decode.string)
    in
    Decode.field "hits" decodeResult


decodeTerms : Decoder (List String)
decodeTerms =
    let
        decodeBuckets =
            Decode.list decodeBucket

        decodeBucket =
            Decode.oneOf
                [ Decode.field "key" Decode.string
                , Decode.field "key" (Decode.map toString Decode.int)
                , Decode.field "key" (Decode.map toString Decode.float)
                ]
    in
    Decode.at [ "aggregations", "genres", "buckets" ] decodeBuckets


esAggregatable : List EsType
esAggregatable =
    [ Text, Ip, Keyword, Date, Integer, Long, HalfFloat, Double ]


fieldNames : NestedKV EsType -> List String
fieldNames values =
    let
        isEsAggregatable ( _, ftype ) =
            List.member ftype esAggregatable

        fieldName ( name, ftype ) =
            case ftype of
                Text ->
                    name ++ ".keyword"

                _ ->
                    name
    in
    values
        |> flattenNestedKV
        |> List.filter isEsAggregatable
        |> List.map fieldName


toEsType : String -> EsType
toEsType str =
    case str of
        "text" ->
            Text

        "ip" ->
            Ip

        "keyword" ->
            Keyword

        "date" ->
            Date

        "integer" ->
            Integer

        "long" ->
            Long

        "geo_point" ->
            GeoPoint

        "half_float" ->
            HalfFloat

        "double" ->
            Double

        _ ->
            Other


flattenNestedKV : NestedKV EsType -> List ( String, EsType )
flattenNestedKV nested =
    let
        handlePrefix prefix name =
            if prefix == "" then
                name
            else
                prefix ++ "." ++ name

        handleChild prefix ( name, et ) acc =
            case et of
                Element ft ->
                    ( handlePrefix prefix name, ft ) :: acc

                Nested children ->
                    List.foldl (handleChild (handlePrefix prefix name)) acc children

        flattenKV prefix acc kvs =
            case kvs of
                Element ft ->
                    ( prefix, ft ) :: acc

                Nested children ->
                    List.foldl (handleChild prefix) acc children
    in
    flattenKV "" [] nested


decodeKVList : Decoder baseType -> Decoder (List ( String, baseType ))
decodeKVList baseDecoder =
    Decode.field "properties" (Decode.keyValuePairs baseDecoder)


decodeNestedKV : Decoder baseType -> Decoder (NestedKV baseType)
decodeNestedKV baseDecoder =
    let
        self =
            Decode.lazy (\_ -> decodeNestedKV baseDecoder)
    in
    Decode.oneOf
        [ decodeKVList self
            |> Decode.map Nested
        , baseDecoder
            |> Decode.map Element
        ]


decodeEsType : Decoder EsType
decodeEsType =
    Decode.field "type" (Decode.map toEsType Decode.string)


decodeFields : Decoder (NestedKV EsType)
decodeFields =
    Decode.field "mappings" <|
        decodeFieldIgnoreKey (Nested []) <|
            decodeNestedKV decodeEsType


decodeIndices : Decoder (NestedKV EsType)
decodeIndices =
    decodeFieldIgnoreKey (Nested []) decodeFields


decodeFieldIgnoreKey : a -> Decoder a -> Decoder a
decodeFieldIgnoreKey default decoder =
    Decode.keyValuePairs decoder
        |> Decode.map
            (List.head
                >> Maybe.withDefault ( "", default )
                >> Tuple.second
            )
