module Page.Profile exposing (view)

import Data.Category as Category exposing (Category)
import Html exposing (Html)
import Html.Attributes as Attributes


view mods =
    let
        viewCategory { name, links } =
            Html.div [ Attributes.class "my-4" ]
                [ Html.p [ Attributes.class "font-semibold text-sm" ]
                    [ Html.text name ]
                , Html.ul []
                    (links |> List.map viewPreviewLink)
                , Html.button [ Attributes.class "text-center block w-full text-gray-700" ]
                    [ Html.text "+ View all" ]
                ]
    in
    Html.div
        [ Attributes.class "flex flex-col justify-center items-center bg-center bg-contain bg-no-repeat"

        -- , Attributes.style "background-image" "url(/images/iphone.png)"
        ]
        [ Html.div
            [ Attributes.style "width" "320px"
            , Attributes.style "max-height" "529px"

            -- , Attributes.style "transform" "scale(0.75)"
            , Attributes.class " overflow-y-scroll border-2 border-black rounded"
            ]
            [ Html.div [ Attributes.class "bg-white" ]
                [ Html.div [ Attributes.class "px-2" ]
                    [ Html.div
                        [ Attributes.style "background-image" <|
                            String.concat
                                [ "url("
                                , "https://www.placecage.com/300/300"
                                , ")"
                                ]
                        , Attributes.class "bg-contain bg-center bg-no-repeat w-20 h-20 mx-auto mt-8 rounded-sm"
                        ]
                        []
                    , Html.h1 [ Attributes.class "text-center font-medium text-lg mt-2" ]
                        [ Html.text "@dwrxht" ]
                    , Html.p [] [ Html.text "2018 wrx premium" ]
                    , Html.p [] [ Html.text "bio?" ]
                    , Html.ul []
                        (mods
                            |> List.map viewCategory
                        )
                    ]
                ]
            ]
        ]


viewPreviewLink { title } =
    Html.li []
        [ Html.div [ Attributes.class "my-3 px-1" ]
            [ Html.div []
                [ Html.a
                    [ Attributes.class "group text-sm md:text-base leading-tight text-center px-2 py-3 border border-green-600 mt-2 block rounded-sm bg-green-500 text-white hover:bg-white hover:text-green-500"
                    , Attributes.href "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"
                    ]
                    [ Html.text title ]
                , Html.p
                    [ Attributes.class "truncate text-xs text-gray-600 px-px py-1 hidden" --hidden for now
                    ]
                    [ Html.text "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob" ]
                ]
            , Html.button [ Attributes.class "block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px" ]
                [ Html.text "···"
                ]
            , Html.div
                [ Attributes.class "border mt-0 px-1 py-2 text-sm text-gray-900"
                , Attributes.classList [ ( "hidden", True ) ]
                ]
                [ Html.text "Here I am adding a comment or whatever. Shoutout to my boy @overlandwrx for the hook up on these sick parts"
                ]
            ]
        ]
