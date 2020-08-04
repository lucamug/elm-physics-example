module Index exposing (htmlToReinject, index)

import Html.String exposing (..)
import Html.String.Attributes exposing (..)
import Html.String.Extra exposing (..)
import Main
import Starter.Flags
import Starter.Icon
import Starter.SnippetHtml
import Starter.SnippetJavascript


index : Starter.Flags.Flags -> Html msg
index flags =
    html
        [ lang "en" ]
        [ head []
            ([]
                ++ [ meta [ charset "utf-8" ] []
                   , title_ [] [ text flags.nameLong ]
                   , meta [ name "author", content flags.author ] []
                   , meta [ name "description", content flags.description ] []
                   , meta [ name "viewport", content "width=device-width, initial-scale=1, shrink-to-fit=no" ] []
                   , meta [ httpEquiv "x-ua-compatible", content "ie=edge" ] []
                   , link [ rel "canonical", href flags.homepage ] []
                   , link [ rel "icon", href (Starter.Icon.iconFileName 64) ] []
                   , link [ rel "apple-touch-icon", href (Starter.Icon.iconFileName 152) ] []
                   , style_ []
                        [ text <| """

                            @font-face {
                                font-family: 'zx_spectrumregular';
                                src: url('zx-spectrum-webfont.woff2') format('woff2'),
                                     url('zx-spectrum-webfont.woff') format('woff');
                                font-weight: normal;
                                font-style: normal;

                            }
                            
                            body 
                                { background-color: """ ++ Starter.Flags.flagsToThemeColor flags ++ """
                                ; font-family: 'zx_spectrumregular'
                                ; margin: 0px;
                                }
                            
                            a
                                { color: white
                                }""" ]
                   ]
                ++ Starter.SnippetHtml.messagesStyle
                ++ Starter.SnippetHtml.pwa (Starter.Flags.flagsToThemeColor flags)
                ++ Starter.SnippetHtml.previewCards flags Main.conf
            )
        , body []
            ([]
                -- Friendly message in case Javascript is disabled
                ++ Starter.SnippetHtml.messageYouNeedToEnableJavascript
                -- "Loading..." message
                ++ Starter.SnippetHtml.messageLoading
                -- The DOM node that Elm will take over
                ++ [ div [ id "elm" ] [] ]
                -- Activating the "Loading..." message
                ++ Starter.SnippetHtml.messageLoadingOn
                -- Loading Elm code
                ++ [ script [ src "/elm.js" ] [] ]
                -- Elm finished to load, de-activating the "Loading..." message
                ++ Starter.SnippetHtml.messageLoadingOff
                -- Loading utility for pretty console formatting
                ++ Starter.SnippetHtml.prettyConsoleFormatting flags.env
                -- Signature "Made with ❤ and Elm"
                ++ [ script [] [ textUnescaped Starter.SnippetJavascript.signature ] ]
                -- Initializing "window.ElmStarter"
                ++ [ script [] [ textUnescaped <| Starter.SnippetJavascript.metaInfo flags ] ]
                -- Let's start Elm!
                ++ [ Html.String.Extra.script []
                        [ Html.String.textUnescaped
                            ("""
                            var node = document.getElementById('elm');
                            window.ElmApp = Elm.Main.init(
                                { node: node
                                , flags:
                                    { starter : """
                                ++ Starter.SnippetJavascript.metaInfoData flags
                                ++ """ 
                                    , width: window.innerWidth
                                    , height: window.innerHeight
                                    , languages: window.navigator.userLanguages || window.navigator.languages || []
                                    , locationHref: location.href
                                    }
                                }
                            );
                            
                            
                            if (ElmApp && ElmApp.ports && ElmApp.ports.changePitch) {
                                ElmApp.ports.changePitch.subscribe(function(newPitch) {
                                    if (source) {
                                        if (! started) {
                                            source.start();
                                            started = true;
                                        }
                                        source.playbackRate.value = 1 + (newPitch / 50);
                                    }
                                });
                            } 
                            
                            
                            // https://www.html5rocks.com/en/tutorials/webaudio/intro/
                            var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
                            var xhr = new XMLHttpRequest();
                            var started = false;
                            var source;
                            var audioData;

                            xhr.open("GET", "engine.wav", true);
                            xhr.responseType = "arraybuffer";
                            xhr.onload = function(e){

                                var audioData = this.response;

                                source = audioCtx.createBufferSource();

                                audioCtx.decodeAudioData(audioData, function(buffer) {
                                    source.buffer = buffer;
                                    source.loop = true;
                                    source.connect(audioCtx.destination);
                                });
                            };

                            xhr.send();
                            """
                                ++ Starter.SnippetJavascript.portOnUrlChange
                                ++ Starter.SnippetJavascript.portPushUrl
                                ++ Starter.SnippetJavascript.portChangeMeta
                            )
                        ]
                   ]
                -- Register the Service Worker, we are a PWA!
                ++ [ script [] [ textUnescaped Starter.SnippetJavascript.registerServiceWorker ] ]
            )
        ]


htmlToReinject : a -> List b
htmlToReinject _ =
    []
