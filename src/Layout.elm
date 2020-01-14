module Layout exposing (..)

import Brand
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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


warningBtn : Element msg -> Element msg
warningBtn content =
    el
        [ Background.color Brand.warningColor
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


textInput : List (Element msg) -> Bool -> Element msg
textInput fields ok =
    row
        [ Brand.underlined 2
        , case ok of
            True ->
                Border.color Brand.secondaryColorMuted

            False ->
                Border.color Brand.warningColor
        , spacing <| Brand.scaled 1
        , focused [ Border.color Brand.primaryColor ]
        , width fill
        ]
        fields


withErrors : Element msg -> Element msg -> Element msg
withErrors field errors =
    column
        [ width fill
        , spacing <| Brand.scaled -5
        , alignTop
        ]
        [ field
        , errors
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


headerText : String -> Element msg
headerText header =
    el
        [ centerX
        , Font.color Brand.primaryTextColorLBg
        , Font.bold
        , Font.size <| Brand.scaled 5
        ]
    <|
        text header


radio : Bool -> Element msg
radio checked =
    el
        [ Border.color Brand.secondaryColorMuted
        , Border.width 2
        , width (px 20)
        , height (px 20)
        , Border.rounded 10
        , padding 3
        , mouseDown [ Background.color Brand.primaryColorMuted ]
        ]
    <|
        if checked then
            el
                [ Background.color Brand.secondaryColorMuted
                , width fill
                , height fill
                , Border.rounded 5
                ]
                Element.none

        else
            Element.none


eye : Bool -> Element msg
eye checked =
    el
        [ width (px 30)
        , height (px 30)
        , Border.rounded 15
        , mouseDown [ Background.color Brand.primaryColorMuted ]
        ]
    <|
        if checked then
            image [ centerX, centerY ]
                { src = "/web/static/assets/visibility-24px.svg"
                , description = "vOn"
                }

        else
            image [ centerX, centerY ]
                { src = "/web/static/assets/visibility_off-24px.svg"
                , description = "vOff"
                }
