module Page.NotFound exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Layout
import Route exposing (Route)
import Session exposing (Session)


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session, Cmd.none )


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


view : Model -> Browser.Document Msg
view model =
    { title = "404 Page Not Found"
    , body =
        [ layout
            [ inFront <| Layout.appBar Element.none
            , Brand.defaultFont
            ]
          <|
            row
                [ Background.color Brand.canvasColor
                , height fill
                , width fill
                , Brand.defaultBodyPadding
                , Font.color Brand.primaryTextColorLBg
                , Font.bold
                ]
                [ el [ centerX ] <| text "404 Page Not Found "
                ]
        ]
    }



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
