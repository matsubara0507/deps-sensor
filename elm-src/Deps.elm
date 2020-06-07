module Deps exposing (Deps, headerView, view)

import Html exposing (..)
import Html.Attributes exposing (class, href)


type alias Deps =
    { repository : String
    , snapshot : String
    }


view : Int -> Deps -> Html msg
view idx deps =
    tr
        [ class "border-top"
        , class
            (if modBy 2 idx == 0 then
                "bg-gray-light"

             else
                ""
            )
        ]
        [ td [ class "text-left p-3" ]
            [ a
                [ class "link-gray-dark"
                , href ("https://github.com/" ++ deps.repository)
                ]
                [ text deps.repository ]
            ]
        , td [ class "text-center p-3" ]
            [ a
                [ class "link-gray-dark"
                , href ("https://www.stackage.org/" ++ deps.snapshot)
                ]
                [ text deps.snapshot ]
            ]
        ]


headerView : List (Html msg)
headerView =
    [ th [ class "text-left p-2" ] [ text "Repository" ]
    , th [ class "text-center p-2" ] [ text "Stackage Snapshot" ]
    ]
