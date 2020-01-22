module Page exposing (view)

import AppBar exposing (Page)
import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Session exposing (Session)


view :
    Session
    -> Maybe AppBar.Menu
    -> Page
    -> { title : String, body : Element bodyMsg }
    -> (AppBar.Msg -> msg)
    -> (bodyMsg -> msg)
    -> Browser.Document msg
view session openMenu page { title, body } appBarMsg bodyMsg =
    { title = "rizzmi/" ++ title
    , body =
        [ layout
            [ AppBar.view session openMenu page
                |> Element.map appBarMsg
                |> inFront
            , Brand.defaultBodyPadding
            , Background.color Brand.canvasColor
            , Brand.defaultFont
            ]
          <|
            Element.map bodyMsg body
        ]
    }
