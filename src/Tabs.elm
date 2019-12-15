module Tabs exposing (Tabs, switchTo)

-- TYPES


type alias Tabs a =
    { before : List a
    , current : a
    , after : List a
    }



-- EXPOSED


switchTo : Tabs a -> a -> Tabs a
switchTo tabs target =
    if tabs.current == target then
        tabs

    else if List.member target tabs.before then
        let
            { before, current, after } =
                splitAt tabs.before target
        in
        Tabs before current (after ++ [ tabs.current ] ++ tabs.after)

    else if List.member target tabs.after then
        let
            { before, current, after } =
                splitAt tabs.after target
        in
        Tabs (tabs.before ++ [ tabs.current ] ++ before) current after

    else
        tabs



-- INTERNAL


splitAt : List a -> a -> Tabs a
splitAt subTabs target =
    let
        head =
            List.head subTabs

        tail =
            List.tail subTabs
    in
    case ( head, tail ) of
        ( Just tab, Just tabs ) ->
            if target == tab then
                Tabs [] tab tabs

            else
                let
                    { before, current, after } =
                        splitAt tabs target
                in
                Tabs ([ tab ] ++ before) current after

        ( Just tab, Nothing ) ->
            Tabs [] tab []

        ( Nothing, Just tabs ) ->
            Tabs [] target tabs

        ( Nothing, Nothing ) ->
            Tabs [] target []
