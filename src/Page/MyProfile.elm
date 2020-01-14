module Page.MyProfile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html as H
import Layout
import Meta exposing (Language(..), Meta, Theme)
import Route exposing (Route)
import Session exposing (Session, meta)
import Tabs exposing (Tabs)
import User exposing (User)
import Viewer



-- MODEL


type alias Form =
    {}


type Model
    = ViewMode Session
    | EditMode Session Form


init : Session -> ( Model, Cmd msg )
init session =
    ( ViewMode session
    , Cmd.none
    )



-- UPDATE


type Msg
    = EditInfo
    | SaveChanges
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditInfo ->
            ( EditMode (toSession model) Form
            , Cmd.none
            )

        SaveChanges ->
            ( ViewMode (toSession model)
            , Cmd.none
            )

        GotSession session ->
            case model of
                ViewMode _ ->
                    ( ViewMode session
                    , Cmd.none
                    )

                EditMode _ form ->
                    ( EditMode session form
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey (toSession model))



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "profile"
    , body =
        case Session.viewer (toSession model) of
            Nothing ->
                Element.none

            Just viewer ->
                column
                    [ paddingXY 0 (Brand.scaled 5)
                    , centerX
                    , width shrink
                    , spacing Brand.defaultPaddingAmount
                    ]
                    [ case model of
                        ViewMode _ ->
                            viewUserInfo <| Viewer.info viewer

                        EditMode _ _ ->
                            viewEditForm <| Viewer.info viewer
                    ]
    }


viewUserInfo : User -> Element Msg
viewUserInfo user =
    Layout.card <|
        row
            [ width (px <| Brand.scaled 19)
            , height shrink
            , Brand.defaultPadding
            , spacing Brand.defaultPaddingAmount
            , Font.color Brand.primaryTextColorLBg
            , alignTop
            ]
            [ viewAvatar
            , column
                [ spacing Brand.defaultPaddingAmount ]
                [ viewName <| User.fullName user
                , viewDetails user
                ]
            , editBtn
            ]


viewEditForm : User -> Element Msg
viewEditForm user =
    Layout.card <|
        row
            [ width (px <| Brand.scaled 19)
            , height shrink
            , Brand.defaultPadding
            , spacing Brand.defaultPaddingAmount
            , Font.color Brand.primaryTextColorLBg
            , alignTop
            ]
            [ viewAvatar
            , column
                [ spacing Brand.defaultPaddingAmount ]
                [ viewName <| User.fullName user
                , viewDetails user
                ]
            , saveBtn
            ]


viewAvatar : Element msg
viewAvatar =
    image
        [ width (px <| Brand.scaled 11)
        , height (px <| Brand.scaled 11)
        , alignTop
        ]
        { src = "/web/static/assets/avatar.svg"
        , description = "Avatar"
        }


viewName : String -> Element msg
viewName name =
    el
        [ Font.size <| Brand.scaled 4
        , Font.bold
        , Font.color Brand.secondaryColor
        ]
        (text <| name)


viewDetails : User -> Element msg
viewDetails user =
    column
        [ spacing <| Brand.scaled -1
        , Font.size <| Brand.scaled 0
        ]
        [ viewInfo
            "email-24px.svg"
            "email"
            user.email
        , viewInfo
            "call-24px.svg"
            "phone number"
            user.phoneNumber
        , viewInfo
            "business-24px.svg"
            "company"
          <|
            Maybe.withDefault "N/A" user.company
        ]


viewInfo : String -> String -> String -> Element msg
viewInfo src description val =
    row
        [ spacing <| Brand.scaled 0 ]
        [ image
            [ centerY ]
            { src = "/web/static/assets/" ++ src
            , description = description
            }
        , el
            [ centerY ]
            (text val)
        ]


editBtn : Element Msg
editBtn =
    Input.button
        [ alignRight
        , alignTop
        ]
        { onPress = Just EditInfo
        , label =
            image
                []
                { src = "/web/static/assets/edit-24px.svg"
                , description = "edit"
                }
        }


saveBtn : Element Msg
saveBtn =
    Input.button
        [ alignRight
        , alignTop
        ]
        { onPress = Just SaveChanges
        , label =
            image
                []
                { src = "/web/static/assets/save-24px.svg"
                , description = "save"
                }
        }



-- EXPORT


toSession : Model -> Session
toSession model =
    case model of
        ViewMode val ->
            val

        EditMode val _ ->
            val
