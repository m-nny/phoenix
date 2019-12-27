module Main exposing (main)

import Browser
import Course exposing (Course, courseListDecoder)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Http


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Failure String
    | Loading
    | Success (List Course)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getCourseList
    )



-- UPDATE


type Msg
    = GotCourseList (Result Http.Error (List Course))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotCourseList result ->
            case result of
                Err error ->
                    ( Failure (httpErrorToString error), Cmd.none )

                Ok courseList ->
                    ( Success courseList, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl msg ->
            msg

        Http.Timeout ->
            "Http: timeout"

        Http.NetworkError ->
            "Http: Network error"

        Http.BadStatus status ->
            "Http: Bad status: " ++ String.fromInt status

        Http.BadBody debug ->
            "Http: Bad body: " ++ debug



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ Background.color (rgb255 28 31 63) ]
        (rootView model)


rootView : Model -> Element Msg
rootView model =
    column rootAttributes
        (searchBar :: coursesView model)


coursesView : Model -> List (Element Msg)
coursesView model =
    case model of
        Loading ->
            [ centeredText "Loading" ]

        Failure err ->
            [ centeredText err ]

        Success courses ->
            List.map courseView courses


rootAttributes : List (Attribute Msg)
rootAttributes =
    [ height fill
    , width (fill |> maximum 600)
    , centerX
    , spacing 32
    , paddingEach { top = 0, left = 0, bottom = 32, right = 0 }
    , Font.family
        [ Font.external
            { name = "Roboto Mono"
            , url = "https://fonts.googleapis.com/css?family=Roboto+Mono:300,400&display=swap"
            }
        , Font.sansSerif
        ]
    , Font.size 14
    , Font.light
    ]


searchBar : Element Msg
searchBar =
    el (card [ height <| px 60 ]) <| title "Course List"


courseView : Course -> Element Msg
courseView { info } =
    column (card [ padding 32, spacing 32 ])
        [ row [ spacing 32, width fill ] [ abbriviatureView info.abbriviature, title info.title, creditsView info.credits ]
        , row [ spacing 32, width fill ] [ schoolView info.school, departmentView info.department ]
        , descriptionView info.description
        ]


abbriviatureView : String -> Element Msg
abbriviatureView abbr =
    paragraph [ Font.alignLeft, width (shrink |> minimum 80) ] [ text (String.replace "/" "\n" abbr) ]


creditsView : Int -> Element msg
creditsView credits =
    paragraph [ Font.alignRight, width (shrink |> minimum 80) ] [ text <| String.fromInt credits ++ "$" ]


schoolView : String -> Element Msg
schoolView school =
    paragraph [ Font.alignLeft ] [ el [ Font.regular ] (text "School: "), text school ]


departmentView : String -> Element Msg
departmentView department =
    paragraph [ Font.alignRight ] [ el [ Font.regular ] (text "Department: "), text department ]


descriptionView : String -> Element Msg
descriptionView desc =
    paragraph [ Font.justify ] [ text desc ]


card : List (Attribute Msg) -> List (Attribute Msg)
card override =
    override
        ++ [ Background.color (rgb255 247 247 255)
           , width fill
           , centerX
           ]


title : String -> Element Msg
title txt =
    paragraph [ centerX, centerY, Font.regular, Font.size 16, Font.center ] [ text txt ]


centeredText : String -> Element Msg
centeredText txt =
    el [ centerX, centerY ] <| text txt


getCourseList : Cmd Msg
getCourseList =
    Http.get
        { url = "https://raw.githubusercontent.com/m-nny/phoenix-api/master/mock/getCourseList.json"
        , expect = Http.expectJson GotCourseList courseListDecoder
        }
