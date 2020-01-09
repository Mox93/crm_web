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


warningColor =
    rgb255 212 15 74


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
            { name = "Ubuntu"
            , url = "https://fonts.googleapis.com/css?family=Ubuntu&display=swap"
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


underlined i =
    Border.widthEach
        { top = 0
        , right = 0
        , bottom = i
        , left = 0
        }


leftMark i =
    Border.widthEach
        { top = 0
        , right = 0
        , bottom = 0
        , left = i
        }
