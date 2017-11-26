module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- View


view : Model -> Html Msg
view model =
    if
        model.loggedin
            == True
            && model.scanoffered
            == False
            && model.gotdata
            == False
            && model.postedresult
            == False
            && model.dismissedresult
            == False
            && model.newresultqueried
            == False
            && model.viewer
            == Off
    then
        div []
            [ button [ onClick OfferScan ] [ text "+*" ]
            , button [] [ text "New Results" ]
            , button [] [ text "View Posted Results" ]
            , button [] [ text "Dismissed Results" ]
            , button [ onClick Logout ] [ text "Logout" ]
            ]
    else if
        model.scanoffered
            == True
            && model.gotdata
            == False
            && model.postedresult
            == False
            && model.dismissedresult
            == False
            && model.newresultqueried
            == False
            && model.viewer
            == Off
    then
        div []
            [ p [] [ text "Scan to allow your lab to send you results" ]
            , img [ src "images/qrcode.jpg" ] []
            , button [ onClick GetData ] [ text "Done" ]
            ]
    else if
        model.scanoffered
            == False
            && model.gotdata
            == True
            && model.postedresult
            == False
            && model.dismissedresult
            == False
            && model.newresultqueried
            == False
            && model.viewer
            == Off
    then
        div []
            [ button [] [ text "+" ]
            , button [ onClick OfferNewResult ] [ text "New Results*" ]
            , button [] [ text "View Posted Results" ]
            , button [] [ text "Dismissed Results" ]
            , button [ onClick Logout ] [ text "Logout" ]
            ]
    else if
        model.scanoffered
            == False
            && model.gotdata
            == False
            && model.postedresult
            == False
            && model.dismissedresult
            == False
            && model.newresultqueried
            == True
            && model.viewer
            == Off
    then
        div []
            [ h1 [] [ text "Only you can see this" ]
            , fullresult
            , h1 [] [ text "Everyone can see this" ]
            , partialresult
            , button [ onClick PostResult ] [ text "Post" ]
            , button [ onClick DismissResult ] [ text "Don't Post" ]
            ]
    else if
        model.scanoffered
            == False
            && model.gotdata
            == False
            && model.postedresult
            == True
            && model.dismissedresult
            == False
            && model.newresultqueried
            == False
            && model.viewer
            == Off
    then
        div []
            [ button [] [ text "+" ]
            , button [] [ text "New Results" ]
            , button [ onClick ActivateViewer ] [ text "View Posted Results*" ]
            , button [] [ text "Dismissed Results" ]
            , button [ onClick Logout ] [ text "Logout" ]
            ]
    else if
        model.scanoffered
            == False
            && model.gotdata
            == False
            && model.postedresult
            == False
            && model.dismissedresult
            == True
            && model.newresultqueried
            == False
            && model.viewer
            == Off
    then
        div []
            [ button [] [ text "+" ]
            , button [] [ text "New Results" ]
            , button [] [ text "View Posted Results" ]
            , button [ onClick OfferNewResult ] [ text "Dismissed Results*" ]
            , button [ onClick Logout ] [ text "Logout" ]
            ]
    else if
        model.scanoffered
            == False
            && model.gotdata
            == False
            && model.postedresult
            == True
            && model.dismissedresult
            == False
            && model.newresultqueried
            == False
            && model.viewer
            == On
            || model.viewer
            == Public
            || model.viewer
            == Private
    then
        if model.viewer == Private then
            div []
                [ h3 [] [ text "View as" ]
                , select [ onInput ChangeView ] [ option [ value "You" ] [ text "You" ], option [ value "Public" ] [ text "Public" ] ]
                , fullresult
                , button [ onClick KillViewer ] [ text "Back" ]
                ]
        else if model.viewer == Public then
            div []
                [ h3 [] [ text "View as" ]
                , select [ onInput ChangeView ] [ option [ value "You" ] [ text "You" ], option [ value "Public" ] [ text "Public" ] ]
                , partialresult
                , button [ onClick KillViewer ] [ text "Back" ]
                ]
        else
            div []
                [ h3 [] [ text "View as" ]
                , select [ onInput ChangeView ] [ option [ value "You" ] [ text "You" ], option [ value "Public" ] [ text "Public" ] ]
                , fullresult
                , button [ onClick KillViewer ] [ text "Back" ]
                ]
    else
        div []
            [ h1 [] [ text "Coral Health" ]
            , button [ onClick Login ] [ text "Start" ]
            ]



-- Model


type Viewer
    = On
    | Off
    | Private
    | Public


type alias Model =
    { loggedin : Bool
    , scanoffered : Bool
    , gotdata : Bool
    , postedresult : Bool
    , dismissedresult : Bool
    , newresultqueried : Bool
    , viewer : Viewer
    }


fullresult : Html Msg
fullresult =
    div []
        [ table []
            [ tr []
                [ th [] [ text "Biomarker" ]
                , th [] [ text "Test Method" ]
                , th [] [ text "Result" ]
                ]
            , tr []
                [ td [] [ text "Estrogen Receptor" ]
                , td [] [ text "IHC" ]
                , td [] [ text "Positive" ]
                ]
            , tr []
                [ td [] [ text "HER2" ]
                , td [] [ text "IHC" ]
                , td [] [ text "Negative" ]
                ]
            , tr []
                [ td [] [ text "BRCA1" ]
                , td [] [ text "NGS" ]
                , td [] [ text "Negative" ]
                ]
            , tr []
                [ td [] [ text "BRCA2" ]
                , td [] [ text "NGS" ]
                , td [] [ text "Negative" ]
                ]
            ]
        ]


partialresult : Html Msg
partialresult =
    div [] [ p [] [ text "Genetic Test - Oncology - October 2017" ] ]



-- Msg


type Msg
    = Login
    | Logout
    | OfferScan
    | GetData
    | OfferNewResult
    | PostResult
    | DismissResult
    | ActivateViewer
    | KillViewer
    | ChangeView String



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( { model | loggedin = True }, Cmd.none )

        Logout ->
            ( { model
                | loggedin = False
                , scanoffered = False
                , gotdata = False
                , postedresult = False
                , dismissedresult = False
                , newresultqueried = False
                , viewer = Off
              }
            , Cmd.none
            )

        OfferScan ->
            ( { model | scanoffered = True }, Cmd.none )

        GetData ->
            ( { model | scanoffered = False, gotdata = True }, Cmd.none )

        OfferNewResult ->
            ( { model | newresultqueried = True, gotdata = False, dismissedresult = False }, Cmd.none )

        PostResult ->
            ( { model | newresultqueried = False, postedresult = True }, Cmd.none )

        DismissResult ->
            ( { model | newresultqueried = False, dismissedresult = True }, Cmd.none )

        ActivateViewer ->
            ( { model | viewer = On }, Cmd.none )

        KillViewer ->
            ( { model | viewer = Off }, Cmd.none )

        ChangeView s ->
            if s == "Public" then
                ( { model | viewer = Public }, Cmd.none )
            else
                ( { model | viewer = Private }, Cmd.none )



-- Init


init : ( Model, Cmd Msg )
init =
    ( { loggedin = False
      , scanoffered = False
      , gotdata = False
      , postedresult = False
      , dismissedresult = False
      , newresultqueried = False
      , viewer = Off
      }
    , Cmd.none
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
