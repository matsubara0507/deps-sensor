module Main exposing (main)

import Browser exposing (Document)
import Deps exposing (Deps)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { dependencies : List Deps
    }


init : { info : List Deps } -> ( Model, Cmd msg )
init flag =
    ( Model flag.info, Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Document msg
view model =
    { title = "Deps Sensor"
    , body = bodyView model
    }


bodyView : Model -> List (Html msg)
bodyView model =
    [ div [ class "my-3 mx-auto container-md" ]
        [ div [ class "Header" ]
            [ div [ class "Header-item Header-item--full" ]
                [ h1 [ class "Header-link" ] [ text "Deps Sensor" ]
                ]
            ]
        , div []
            [ table
                [ class "col-12 f2 break-word", style "table-layout" "fixed" ]
                [ thead [] [ tr [ class "border-bottum" ] Deps.headerView ]
                , tbody [] (List.indexedMap Deps.view model.dependencies)
                ]
            ]
        ]
    ]
