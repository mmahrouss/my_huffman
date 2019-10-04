module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict
import Html exposing (Attribute, Html, button, div, input, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import MyHuffman



-- import MyHuffman
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String --String to Encode.
    , table : List ( String, Float, String ) --encoding table
    , encode : Bool --Flag to Calculate only on clicking encode
    }


init : Model
init =
    { content = ""
    , table = []
    , encode = False
    }



-- UPDATE


type Msg
    = Change String
    | Encode --should be sent after clicking an encode button


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent, encode = False }

        Encode ->
            { model
                | encode = True
                , table =
                    case MyHuffman.get_huffman_table (.content model) of
                        Just m ->
                            m

                        Nothing ->
                            .table model
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to encode", value model.content, onInput Change ] []
        , div [] [ text model.content ]
        , button [ onClick Encode ] [ text "Encode" ]
        , viewValidation model
        ]


viewValidation : Model -> Html msg
viewValidation model =
    if model.encode then
        table
            [ style "width" "75%" ]
            (tr
                []
                [ th [] [ text "Symbol" ]
                , th [] [ text "Probability" ]
                , th [] [ text "Code" ]
                ]
                :: table2rows (.table model)
            )

    else
        table [] []


table2rows_helper : List ( String, Float, String ) -> List (Html msg) -> List (Html msg)
table2rows_helper l html_row_list =
    case l of
        ( symbol, prob, code ) :: ltail ->
            table2rows_helper ltail
                (tr
                    []
                    [ th [] [ text symbol ]
                    , th [] [ text (String.fromFloat prob) ]
                    , th [] [ text code ]
                    ]
                    :: html_row_list
                )

        [] ->
            html_row_list


table2rows : List ( String, Float, String ) -> List (Html msg)
table2rows l =
    table2rows_helper l []
