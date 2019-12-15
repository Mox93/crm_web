module Meta exposing (..)

import Time



-- TYPES


type Language
    = English
    | Arabic


type Theme
    = Default
    | Dark
    | Light
      -- TODO shouldn't take just a string
    | Custom


type alias Meta =
    { language : Language
    , theme : Theme
    , timeZone : Time.Zone
    }


defaults : Meta
defaults =
    Meta English Default Time.utc
