module Menu exposing (..)

import Element exposing (Element)


type DropDown content
    = Opened content
    | Closed content


toggleMenu : DropDown content -> DropDown content
toggleMenu menu =
    case menu of
        Opened val ->
            Closed val

        Closed val ->
            Opened val


viewMenu : DropDown (Element msg) -> Element msg
viewMenu menu =
    case menu of
        Opened list ->
            list

        Closed _ ->
            Element.none
