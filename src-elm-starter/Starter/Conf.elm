module Starter.Conf exposing
    ( Conf
    , conf
    )

import Html.String
import Html.String.Extra
import Index
import Json.Encode
import Main
import Starter.ConfMain
import Starter.ConfMeta
import Starter.ElmLive
import Starter.Flags
import Starter.Icon
import Starter.Manifest
import Starter.ServiceWorker


file : ( String, String ) -> Json.Encode.Value
file ( name, content_ ) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name )
        , ( "content", Json.Encode.string content_ )
        ]


fileIndexHtml :
    String
    -> Int
    -> Html.String.Html msg
    -> String
fileIndexHtml messageDoNotEditDisclaimer indentation startingPage =
    Html.String.Extra.doctype
        ++ "\n"
        ++ "<!-- "
        ++ messageDoNotEditDisclaimer
        ++ " -->\n"
        ++ Html.String.toString indentation
            startingPage


type alias Conf msg =
    { dir : Starter.Flags.Dir
    , file : Starter.Flags.File
    , fileNames : Starter.ConfMeta.FileNames
    , fileIndexHtml : Html.String.Html msg
    , htmlToReinject : List (Html.String.Html msg)
    , iconsForManifest : List Int
    , portBuild : Int
    , portDev : Int
    , portStatic : Int
    , messageDoNotEditDisclaimer : String
    , flags : Starter.Flags.Flags
    }


conf : Starter.Flags.Flags -> Json.Encode.Value
conf flags =
    encoder
        { dir = Starter.Flags.dir flags
        , file = Starter.Flags.file flags
        , fileNames = Starter.ConfMeta.conf.fileNames
        , fileIndexHtml = Index.index flags
        , htmlToReinject = Index.htmlToReinject flags
        , iconsForManifest = Starter.Icon.iconsForManifest
        , portBuild = Starter.ConfMeta.conf.portBuild
        , portDev = Starter.ConfMeta.conf.portDev
        , portStatic = Starter.ConfMeta.conf.portStatic
        , messageDoNotEditDisclaimer = Starter.ConfMeta.conf.messageDoNotEditDisclaimer
        , flags = flags
        }


encoder : Conf msg -> Json.Encode.Value
encoder args =
    Json.Encode.object
        [ ( "dir"
          , args.dir
                |> Starter.Flags.dirEncoder
          )
        , ( "file"
          , args.file
                |> Starter.Flags.fileEncoder
          )
        , ( "serverDev"
          , { elmFileToCompile = .mainElm (Starter.Flags.file args.flags)
            , dir = .dev (Starter.Flags.dir args.flags)
            , outputCompiledJs = .dev (Starter.Flags.dir args.flags) ++ args.fileNames.outputCompiledJs
            , indexHtml = args.fileNames.indexHtml
            , port_ = args.portDev
            , compilation =
                case args.flags.developmentWithoutDebugger of
                    Just True ->
                        Starter.ElmLive.Normal

                    _ ->
                        Starter.ElmLive.Debug
            , verbose = Starter.ElmLive.VerboseNo
            , reload = Starter.ElmLive.ReloadYes
            , hotReload = Starter.ElmLive.HotReloadYes
            , dirBin = .bin (Starter.Flags.dir args.flags)
            }
                |> Starter.ElmLive.elmLive
                |> Starter.ElmLive.encoder
          )
        , ( "serverStatic"
          , { elmFileToCompile = .mainElm (Starter.Flags.file args.flags)
            , dir = .dev (Starter.Flags.dir args.flags)
            , outputCompiledJs = .dev (Starter.Flags.dir args.flags) ++ args.fileNames.outputCompiledJs
            , indexHtml = args.fileNames.indexHtml
            , port_ = args.portStatic
            , compilation = Starter.ElmLive.Optimize
            , verbose = Starter.ElmLive.VerboseNo
            , reload = Starter.ElmLive.ReloadNo
            , hotReload = Starter.ElmLive.HotReloadNo
            , dirBin = .bin (Starter.Flags.dir args.flags)
            }
                |> Starter.ElmLive.elmLive
                |> Starter.ElmLive.encoder
          )
        , ( "serverBuild"
          , { elmFileToCompile = .mainElm (Starter.Flags.file args.flags)
            , dir = .build (Starter.Flags.dir args.flags)
            , outputCompiledJs = .dev (Starter.Flags.dir args.flags) ++ args.fileNames.outputCompiledJs
            , indexHtml = args.fileNames.indexHtml
            , port_ = args.portBuild
            , compilation = Starter.ElmLive.Normal
            , verbose = Starter.ElmLive.VerboseNo
            , reload = Starter.ElmLive.ReloadNo
            , hotReload = Starter.ElmLive.HotReloadNo
            , dirBin = .bin (Starter.Flags.dir args.flags)
            }
                |> Starter.ElmLive.elmLive
                |> Starter.ElmLive.encoder
          )
        , ( "headless", Json.Encode.bool True )
        , ( "startingDomain"
          , Json.Encode.string
                ("http://localhost:" ++ String.fromInt args.portStatic)
          )
        , ( "batchesSize", Json.Encode.int 4 )
        , ( "pagesName", Json.Encode.string "index.html" )
        , ( "snapshots", Json.Encode.bool True )
        , ( "snapshotsQuality", Json.Encode.int 80 )
        , ( "snapshotWidth", Json.Encode.int <| Maybe.withDefault 700 <| String.toInt <| Maybe.withDefault "" <| args.flags.snapshotWidth )
        , ( "snapshotHeight", Json.Encode.int <| Maybe.withDefault 350 <| String.toInt <| Maybe.withDefault "" <| args.flags.snapshotHeight )
        , ( "snapshotFileName", Json.Encode.string Starter.ConfMeta.conf.fileNames.snapshot )
        , ( "mainConf", Starter.ConfMain.encoder Main.conf )
        , ( "htmlToReinject"
          , args.htmlToReinject
                |> List.map (\html -> Html.String.toString Starter.ConfMeta.conf.indentation html)
                |> String.join ""
                |> Json.Encode.string
          )
        , ( "files"
          , (Json.Encode.list <| file)
                --
                -- "/manifest.json"
                --
                [ ( args.fileNames.manifestJson
                  , { iconSizes = args.iconsForManifest
                    , themeColor = Starter.Flags.flagsToThemeColor args.flags
                    , name = args.flags.name
                    , nameLong = args.flags.nameLong
                    }
                        |> Starter.Manifest.manifest
                        |> Json.Encode.encode Starter.ConfMeta.conf.indentation
                  )

                -- "/_redirects"
                --
                -- Netlify Configuration File
                --
                , ( args.fileNames.redirects
                  , "/* /index.html 200"
                  )

                -- "/service-worker.js"
                --
                , ( args.fileNames.serviceWorker
                  , Starter.ServiceWorker.serviceWorker
                  )

                -- "/index.html"
                --
                , ( args.fileNames.indexHtml
                  , fileIndexHtml args.messageDoNotEditDisclaimer Starter.ConfMeta.conf.indentation args.fileIndexHtml
                  )

                -- "/robots.txt"
                --
                -- https://www.robotstxt.org/robotstxt.html
                --
                , ( args.fileNames.robotsTxt
                  , [ "User-agent: *"
                    , "Disallow:"
                    , "Sitemap: " ++ args.flags.homepage ++ args.fileNames.sitemap
                    ]
                        |> String.join "\n"
                  )

                -- "/sitemap.txt"
                --
                , ( args.fileNames.sitemap
                  , String.join "\n" <| List.map (\url -> args.flags.homepage ++ url) Main.conf.urls
                  )
                ]
          )
        ]
