module Asset exposing
    ( Image
    , avatar
    , backArrow
    , bell
    , company
    , defaultAvatar
    , edit
    , email
    , gear
    , image
    , invisible
    , logo
    , phone
    , save
    , src
    , visible
    )

{-| Assets, such as images, videos, and audio. (We only have images for now.)
We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!
-}


type Image
    = Image String



-- IMAGES


backArrow : Image
backArrow =
    asset "arrow_back_ios-24px.svg"


edit : Image
edit =
    asset "edit-24px.svg"


save : Image
save =
    asset "save-24px.svg"


email : Image
email =
    asset "email-24px.svg"


phone : Image
phone =
    asset "call-24px.svg"


company : Image
company =
    asset "business-24px.svg"


logo : Image
logo =
    asset "logo.svg"


bell : Image
bell =
    asset "notifications-24px.svg"


gear : Image
gear =
    asset "settings_applications-24px.svg"


invisible : Image
invisible =
    asset "visibility_off-24px.svg"


visible : Image
visible =
    asset "visibility-24px.svg"


defaultAvatar : Image
defaultAvatar =
    Image "/avatar/default.svg"



-- INTERNAL


asset : String -> Image
asset filename =
    Image ("/assets/" ++ filename)



-- USING IMAGES


avatar : String -> Image
avatar filename =
    Image ("/avatar/" ++ filename)


image : String -> Image
image filename =
    Image ("/img/" ++ filename)


src : Image -> String
src (Image url) =
    url
