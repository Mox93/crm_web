module Config exposing (..)

import Time



-- TYPES


type Language
    = English
    | Arabic


type Theme
    = Light
    | Dark


type alias Config =
    { language : Language
    , theme : Theme
    , timeZone : Time.Zone
    }


defaults : Config
defaults =
    Config English Light Time.utc
