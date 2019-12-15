module Brand exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- COLOR


primaryColor =
    rgb255 80 161 65


primaryColorMuted =
    rgb255 143 189 95


canvasColor =
    rgb255 114 203 204


cardColor =
    rgb255 244 244 244


transparent =
    rgba255 0 0 0 0


shadowColor =
    rgba255 0 0 0 0.05


secondaryColor =
    rgb255 72 71 70


secondaryColorMuted =
    rgb255 125 124 122


toElmColor =
    Element.toRgb >> Color.fromRgba



-- TEXT


primaryTextColorDBg =
    cardColor


primaryTextColorLBg =
    rgb255 16 16 16


subtleTextColor =
    secondaryColorMuted


defaultFont =
    Font.family
        [ Font.external
            { name = "Noto Sans JP"
            , url = "https://fonts.googleapis.com/css?family=Noto+Sans+JP&display=swap"
            }
        , Font.sansSerif
        ]



-- SPACING


defaultPadding =
    padding defaultPaddingAmount


defaultPaddingAmount =
    scaled 2


defaultBodyPadding =
    Element.paddingEach
        { top = appBarHeight + defaultPaddingAmount
        , left = defaultPaddingAmount
        , right = defaultPaddingAmount
        , bottom = defaultPaddingAmount
        }


defaultBarPadding =
    Element.paddingXY defaultPaddingAmount 4


appBarHeight =
    50


scaled power =
    round (Element.modular 16 1.25 power)



-- SHAPES


shadow =
    Border.shadow
        { offset = ( 1, 1 )
        , size = 2
        , blur = 4
        , color = Element.rgba255 0 0 0 0.2
        }


bottomBorder i =
    Border.widthEach
        { top = 0
        , right = 0
        , bottom = i
        , left = 0
        }


leftBorder i =
    Border.widthEach
        { top = 0
        , right = 0
        , bottom = 0
        , left = i
        }


radio : Bool -> Element msg
radio checked =
    el
        [ Border.color secondaryColorMuted
        , Border.width 2
        , width (px 20)
        , height (px 20)
        , Border.rounded 10
        , padding 3
        , mouseDown [ Background.color primaryColorMuted ]
        ]
    <|
        if checked then
            el
                [ Background.color secondaryColorMuted
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
        , mouseDown [ Background.color primaryColorMuted ]
        ]
    <|
        if checked then
            image [ centerX, centerY ]
                { src = "assets/visibility-24px.svg"
                , description = "vOn"
                }

        else
            image [ centerX, centerY ]
                { src = "assets/visibility_off-24px.svg"
                , description = "vOff"
                }
