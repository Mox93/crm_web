module Page.NotFound exposing (..)

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


init : Session -> Model
init session =
    { session = session }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


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
