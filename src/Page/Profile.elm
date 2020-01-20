module Page.Profile exposing (view)

import Data.Category as Category exposing (Category)
import Data.User as User exposing (DriverProfile)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Extra as Html


view mods profile =
    Html.div
        [ Attributes.class "flex flex-col justify-center items-center bg-center bg-contain bg-no-repeat"
        ]
        [ Html.div
            [ Attributes.style "width" "320px"
            , Attributes.style "height" "529px"
            , Attributes.style "transform" "scale(0.75)"
            , Attributes.class " overflow-y-scroll border-2 border-black rounded max-w-full max-w-full"
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
                        [ Html.text <| String.concat [ "@", profile.username ] ]
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


viewCategory { name, links } =
    let
        firstChunk =
            List.take 3 links

        theRest =
            List.drop 3 links
    in
    Html.div [ Attributes.class "my-4" ]
        [ Html.p [ Attributes.class "font-semibold text-sm" ]
            [ Html.text name ]
        , Html.ul []
            (firstChunk |> List.map viewPreviewLink)
        , Html.viewIf (not (List.isEmpty theRest)) <|
            Html.node "ui-openable"
                []
                [ Html.button
                    [ Attributes.class "text-center block w-full text-gray-700"
                    , Attributes.attribute "Openable__activator" ""
                    ]
                    [ Html.text "+ View all" ]
                , Html.ul
                    [ Attributes.class "hidden"
                    , Attributes.attribute "Openable__content" ""
                    ]
                    (List.map viewPreviewLink theRest)
                ]
        ]



-- custom element opanable


viewPreviewLink { title, description } =
    Html.li []
        [ Html.div [ Attributes.class "my-3 px-1" ]
            [ Html.div []
                [ Html.a
                    [ Attributes.class "group text-sm md:text-base leading-tight text-center px-2 py-3 border border-green-600 mt-2 block rounded-sm bg-green-500 text-white hover:bg-white hover:text-green-500"
                    , Attributes.href "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"
                    ]
                    [ Html.text title ]
                ]
            , Html.viewIf (not (String.isEmpty description)) <|
                Html.node "ui-openable"
                    []
                    [ Html.button
                        [ Attributes.class "block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px"
                        , Attributes.attribute "Openable__activator" ""
                        ]
                        [ Html.text "···"
                        ]
                    , Html.div
                        [ Attributes.class "border mt-0 px-1 py-2 text-sm text-gray-900"
                        , Attributes.classList [ ( "hidden", True ) ]
                        , Attributes.attribute "Openable__content" ""
                        ]
                        [ Html.p [] [ Html.text description ]
                        ]
                    , Html.button
                        [ Attributes.class "hidden block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px"
                        , Attributes.attribute
                            "Openable__deactivator"
                            ""
                        ]
                        [ Html.text "Close" ]
                    ]
            ]
        ]
