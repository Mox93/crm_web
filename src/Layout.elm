module Layout exposing (..)

import Asset
import Brand
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ErrorHandling exposing (Problem, problemToString)
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
        , Border.color <|
            if ok then
                Brand.secondaryColorMuted

            else
                Brand.warningColor
        , spacing <| Brand.scaled 1
        , focused [ Border.color Brand.primaryColor ]
        , width fill
        , Font.color <|
            if ok then
                Brand.primaryTextColorLBg

            else
                Brand.warningColor
        ]
        fields


withErrors : Element msg -> List (Problem validatedField) -> Element msg
withErrors field problems =
    column
        [ width fill
        , spacing <| Brand.scaled -5
        , alignTop
        ]
    <|
        case List.isEmpty problems of
            True ->
                [ field ]

            False ->
                [ field
                , errNote problems
                ]


errNote : List (Problem validatedField) -> Element msg
errNote problems =
    column
        [ Font.color Brand.warningColor
        , Font.size <| Brand.scaled -1
        , width (maximum (Brand.scaled 15) fill)
        ]
    <|
        List.map
            (\err ->
                paragraph
                    []
                    [ text <| "* " ++ problemToString err ]
            )
            problems


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


card : List (Attr () msg) -> Element msg -> Element msg
card attr contents =
    el
        (List.append
            [ Background.color Brand.cardColor
            , Brand.defaultPadding
            , Border.rounded <| Brand.scaled 1
            , centerY
            , centerX
            , Brand.shadow
            ]
            attr
        )
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


radio : Bool -> Bool -> Element msg
radio ok checked =
    el
        [ Border.color <|
            if ok then
                Brand.secondaryColorMuted

            else
                Brand.warningColor
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
                [ Background.color <|
                    if ok then
                        Brand.secondaryColorMuted

                    else
                        Brand.warningColor
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
                { src = Asset.src Asset.visible
                , description = "vOn"
                }

        else
            image [ centerX, centerY ]
                { src = Asset.src Asset.invisible
                , description = "vOff"
                }


logoutBtn : Element msg
logoutBtn =
    link [ centerY ]
        { url = Route.toString Route.Logout
        , label = secondaryBan <| text "Logout"
        }
