module Page.MyProfile exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Asset exposing (Image)
import Avatar exposing (Avatar)
import Brand
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Layout
import Route
import Session exposing (Session)
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
                    , Route.replaceUrl (Session.navKey session) Route.MyProfile
                    )

                EditMode _ form ->
                    ( EditMode session form
                    , Route.replaceUrl (Session.navKey session) Route.MyProfile
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey (toSession model))



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "My Profile"
    , body =
        case Session.viewer (toSession model) of
            Nothing ->
                Element.none

            Just viewer ->
                column
                    [ paddingXY 0 (Brand.scaled 5)
                    , centerX
                    , width
                        (fill
                            |> maximum (Brand.scaled 19)
                            |> minimum (Brand.scaled 17)
                        )
                    , spacing Brand.defaultPaddingAmount
                    ]
                    [ case model of
                        ViewMode _ ->
                            viewUserInfo <| Viewer.info viewer

                        EditMode _ _ ->
                            viewEditForm <| Viewer.info viewer
                    , Layout.card
                        [ width fill
                        , height (px 200)
                        ]
                        Element.none
                    ]
    }


viewUserInfo : User -> Element Msg
viewUserInfo user =
    Layout.card
        [ width fill ]
    <|
        row
            [ Brand.defaultPadding
            , spacing Brand.defaultPaddingAmount
            , Font.color Brand.primaryTextColorLBg
            , alignTop
            , width fill
            ]
            [ viewAvatar user.avatar
            , column
                [ spacing Brand.defaultPaddingAmount ]
                [ viewName <| User.fullName user
                , viewDetails user
                ]
            , editBtn
            ]


viewEditForm : User -> Element Msg
viewEditForm user =
    Layout.card
        [ width fill ]
    <|
        row
            [ Brand.defaultPadding
            , spacing Brand.defaultPaddingAmount
            , Font.color Brand.primaryTextColorLBg
            , alignTop
            , width fill
            ]
            [ viewAvatar user.avatar
            , column
                [ spacing Brand.defaultPaddingAmount ]
                [ viewName <| User.fullName user
                , viewDetails user
                ]
            , saveBtn
            ]


viewAvatar : Avatar -> Element msg
viewAvatar avatar =
    el [ alignTop ] <|
        Avatar.view avatar <|
            Brand.scaled 10


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
            Asset.email
            "email"
            user.email
        , viewInfo
            Asset.phone
            "phone number"
            user.phoneNumber
        , viewInfo
            Asset.company
            "company"
          <|
            Maybe.withDefault "N/A" user.company
        ]


viewInfo : Image -> String -> String -> Element msg
viewInfo img description val =
    row
        [ spacing <| Brand.scaled 0 ]
        [ image
            [ centerY ]
            { src = Asset.src img
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
                { src = Asset.src Asset.edit
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
                { src = Asset.src Asset.save
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
