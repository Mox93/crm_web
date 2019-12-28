module Layout exposing (..)

import Brand
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Route


type alias RootAppBar msg =
    { title : String
    , actions : List (Element msg)
    }


type alias BranchAppBar msg =
    { backRoute : Route.Route
    , actions : List (Element msg)
    }


type AppBarConfig msg
    = Root (RootAppBar msg)
    | Branch (BranchAppBar msg)


signupBtn : Element msg
signupBtn =
    primaryBtn <| text "Sign up for free"


loginBtn : Element msg
loginBtn =
    secondaryBan <| text "Login"


primaryBtn : Element msg -> Element msg
primaryBtn content =
    el
        [ Background.color Brand.primaryColor
        , Font.color Brand.primaryTextColorDBg
        , Font.size <| Brand.scaled 1
        , Border.rounded 6
        , paddingXY 12 8
        , Element.pointer
        ]
        content


secondaryBan : Element msg -> Element msg
secondaryBan content =
    el
        [ Border.color Brand.cardColor
        , Border.width 2
        , Font.color Brand.primaryTextColorDBg
        , Font.size <| Brand.scaled 1
        , Border.rounded 6
        , paddingXY 10 6
        , Element.pointer
        ]
        content


textInput : List (Element msg) -> Element msg
textInput fields =
    column
        [ width (px <| Brand.scaled 15) ]
        [ row
            [ Brand.bottomBorder 2
            , Border.color Brand.secondaryColorMuted
            , spacing <| Brand.scaled 1
            , focused [ Border.color Brand.primaryColor ]
            ]
            fields
        ]


scrollBar : Float -> (Float -> msg) -> Element msg
scrollBar loc scroll =
    Input.slider
        [ height fill
        , width (Element.px 15)

        -- Here is where we're creating/styling the "track"
        , behindContent
            (el
                [ width (px 5)
                , height fill
                , centerX
                , Background.color Brand.secondaryColorMuted
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = scroll
        , label = Input.labelHidden "My Slider Value"
        , min = 0
        , max = 1
        , step = Nothing
        , value = loc
        , thumb = Input.defaultThumb
        }


card : Element msg -> Element msg
card contents =
    el
        [ Background.color Brand.cardColor
        , Brand.defaultPadding
        , Border.rounded <| Brand.scaled 1
        , centerY
        , centerX
        , Brand.shadow
        ]
        contents


viewHeader : String -> Element msg
viewHeader header =
    el
        [ centerX
        , Font.color Brand.primaryTextColorLBg
        , Font.bold
        , Font.size <| Brand.scaled 5
        ]
    <|
        text header


appBar : Element msg -> Element msg
appBar content =
    row
        [ width fill
        , height (Element.px Brand.appBarHeight)
        , Brand.defaultBarPadding
        , Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Brand.shadow
        ]
        [ link [ Font.bold ]
            { url = Route.toString Route.Home
            , label =
                image
                    [ height (px <| Brand.scaled 3) ]
                    { src = "assets/logo.svg"
                    , description = "rizzmi"
                    }
            }
        , content
        ]
