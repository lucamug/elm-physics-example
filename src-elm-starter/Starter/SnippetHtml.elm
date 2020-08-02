module Starter.SnippetHtml exposing
    ( messageEnableJavascriptForBetterExperience
    , messageLoading
    , messageLoadingOff
    , messageLoadingOn
    , messageYouNeedToEnableJavascript
    , messagesStyle
    , prettyConsoleFormatting
    , previewCards
    , pwa
    )

import Html.String exposing (..)
import Html.String.Attributes exposing (..)
import Html.String.Extra exposing (..)
import Starter.ConfMain
import Starter.ConfMeta
import Starter.Flags
import Starter.SnippetCss


{-| PWA stuff
-}



-- pwa : List (Html msg)


pwa : String -> List (Html msg)
pwa themeColor =
    -- DNS preconnect and prefetch for
    -- https://storage.googleapis.com/workbox-cdn/releases/5.1.2/workbox-sw.js
    [ link [ rel "preconnect", href "https://storage.googleapis.com", crossorigin "true" ] []
    , link [ rel "dns-prefetch", href "https://storage.googleapis.com" ] []

    -- PWA
    , meta [ name "theme-color", content themeColor ] []
    , meta [ name "mobile-web-app-capable", content "yes" ] []
    , link [ rel "manifest", href Starter.ConfMeta.conf.fileNames.manifestJson ] []

    -- iOS
    , meta [ name "apple-mobile-web-app-capable", content "yes" ] []
    , meta [ name "apple-mobile-web-app-status-bar-style", content "black" ] []
    , meta [ name "apple-mobile-web-app-title", content "Test PWA" ] []
    ]


{-| Mix of Twitter and Open Graph tags to define a summary card

<https://developer.twitter.com/en/docs/tweets/optimize-with-cards/guides/getting-started>

-}



-- previewCards : List (Html msg)


previewCards : Starter.Flags.Flags -> Starter.ConfMain.Conf -> List (Html msg)
previewCards flags mainConf =
    --
    -- From https://medium.com/slack-developer-blog/everything-you-ever-wanted-to-know-about-unfurling-but-were-afraid-to-ask-or-how-to-make-your-e64b4bb9254
    --
    -- facebook open graph tags
    []
        ++ [ meta [ property_ "og:type", content "website" ] []
           , meta [ property_ "og:url", content flags.homepage ] []
           , meta [ property_ "og:title", content flags.nameLong ] []
           , meta [ property_ "og:description", content flags.description ] []
           , meta [ property_ "og:image", content Starter.ConfMeta.conf.fileNames.snapshot ] []

           -- twitter card tags additive with the og: tags
           , meta [ name "twitter:card", content "summary_large_image" ] []
           ]
        ++ (case flags.twitterSite of
                Just twitterSite ->
                    [ meta [ name "twitter:site", content ("@" ++ twitterSite) ] [] ]

                Nothing ->
                    []
           )
        ++ (case flags.twitterAuthor of
                Just twitterAuthor ->
                    [ meta [ name "twitter:site", content ("@" ++ twitterAuthor) ] [] ]

                Nothing ->
                    []
           )
        ++ [ meta [ name "twitter:domain", value flags.homepage ] []
           , meta [ name "twitter:title", value flags.nameLong ] []
           , meta [ name "twitter:description", value flags.description ] []
           , meta [ name "twitter:image", content Starter.ConfMeta.conf.fileNames.snapshot ] []
           , meta [ name "twitter:url", value flags.homepage ] []

           -- , meta [ name "twitter:label1", value "Opens in Theaters" ] []
           -- , meta [ name "twitter:data1", value "December 1, 2015" ] []
           -- , meta [ name "twitter:label2", value "Or on demand" ] []
           -- , meta [ name "twitter:data2", value "at Hulu.com" ] []
           ]


prettyConsoleFormatting : String -> List (Html msg)
prettyConsoleFormatting env =
    if env == "dev" then
        [ script [ src "/assets-dev/elm-console-debug.js" ] []
        , script [] [ textUnescaped "ElmConsoleDebug.register()" ]
        ]

    else
        []


messageYouNeedToEnableJavascript : List (Html msg)
messageYouNeedToEnableJavascript =
    [ noscript []
        [ div
            [ class Starter.ConfMeta.conf.tagNotification
            , style "top" "0"
            , style "height" "100vh"
            ]
            [ text Starter.ConfMeta.conf.messageYouNeedToEnableJavascript ]
        ]
    ]


messageEnableJavascriptForBetterExperience : List (Html msg)
messageEnableJavascriptForBetterExperience =
    [ noscript []
        [ div
            [ class Starter.ConfMeta.conf.tagNotification
            , style "bottom" "0"
            ]
            [ text Starter.ConfMeta.conf.messageEnableJavascriptForBetterExperience ]
        ]
    ]


messageLoading : List (Html msg)
messageLoading =
    [ div
        [ id Starter.ConfMeta.conf.tagLoader
        , class Starter.ConfMeta.conf.tagNotification
        , style "height" "100vh"
        , style "display" "none"
        ]
        [ text Starter.ConfMeta.conf.messageLoading ]
    ]


messageLoadingOn : List (Html msg)
messageLoadingOn =
    [ script []
        [ textUnescaped <|
            "document.getElementById('"
                ++ Starter.ConfMeta.conf.tagLoader
                ++ "').style.display = 'block';"
        ]
    ]


messageLoadingOff : List (Html msg)
messageLoadingOff =
    [ script []
        [ textUnescaped <|
            "document.getElementById('"
                ++ Starter.ConfMeta.conf.tagLoader
                ++ "').style.display = 'none';"
        ]
    ]


messagesStyle : List (Html msg)
messagesStyle =
    [ style_ []
        [ text <|
            Starter.SnippetCss.noJsAndLoadingNotifications
                Starter.ConfMeta.conf.tagNotification
        ]
    ]
