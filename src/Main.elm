module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Data
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (alt, attribute, class, css, disabled, href, src, target)
import Html.Styled.Events exposing (onClick)
import Http
import Markdown
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
import Url exposing (Url)
import View exposing (navIn, navOut)


type alias Flags =
    {}



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , package : RemoteData Http.Error Data.Package
    }


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
      BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserClickedPackageButton
    | ServerRespondedWithPackage (Result Http.Error Data.Package)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


project : { description : String, title : String, url : String }
project =
    { title = "concat.dev"
    , description = """
        Learn Functional Programming. Learn Reactive Programming.
      """
    , url = "https://github.com/cedricss/elm-batteries#elm-batteries-included"
    }


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model |> div [] |> toUnstyled ]
    }


body : Model -> List (Html Msg)
body model =
    [ View.header
        [ navIn "RxJS" "/rxjs"
        , navIn "Elm" "/elm"
        , navOut "Twitter" "https://twitter.com/CedricSoulas"
        ]
    , View.container <|
        case model.route of
            Elm ->
                viewElm model

            RxJS ->
                viewRxJS model

            Home ->
                viewHome model

            NotFound ->
                View.notFound
    ]


viewContent : List (Html Msg) -> Html Msg
viewContent content =
    div
        [ class "content text-center" ]
        content


viewHome : Model -> List (Html Msg)
viewHome model =
    [ h1
        [ class "flex items-center" ]
        [ text "Coming soon" ]
    , p
        [ class "bg-gray-200 px-6 py-4 rounded max-w-xl mb-4"
        , class "text-xl"
        ]
        [ a [ href "https://twitter.com/CedricSoulas", target "_blank" ] [ text "Cédric Soulas" ]
        , text " is teaching Functional Programming and Reactive Programming."
        ]
    , ul
        [ class "text-xl" ]
        [ li
            []
            [ a
                [ href "/rxjs" ]
                [ text "Learn RxJS ›" ]
            ]
        , li []
            [ a
                [ href "/elm" ]
                [ text "Learn Elm ›" ]
            ]
        ]
    ]


viewRxJS : Model -> List (Html Msg)
viewRxJS model =
    [ h1 [] [ text "RxJS" ]
    , h2 [] [ text "Learn Reactive Programming and RxJS" ]
    , ul
        [ class "text-xl" ]
        [ li
            []
            [ a
                [ href "https://reactive.how", target "_blank" ]
                [ text "reactive.how ›" ]
            ]
        , li
            []
            [ a
                [ href "https://reactive.how/rxjs", target "_blank" ]
                [ text "Launchpad for RxJS ›" ]
            ]
        , li
            []
            [ a
                [ href "https://www.humancoders.com/formations/rxjs", target "_blank" ]
                [ text "RxJS workshop ›" ]
            ]
        ]
    ]


viewElm : Model -> List (Html Msg)
viewElm model =
    [ h1 [] [ text "Elm" ]
    , h2 [] [ text "Learn Functional Programming and Elm" ]
    , p
        [ class "my-8 bg-gray-200 px-8 py-6 rounded max-w-lg" ]
        [ h3
            [ class "mb-2 max-w-lg" ]
            [ text "Receive my latest news and tips about Elm" ]
        , a
            [ class "btn mb-2"
            , attribute "data-action" "fetch-package"
            , href "#"
            ]
            [ text "Subscribe to the mailing list  ›" ]
        , br [] []
        , text "or follow "
        , a
            [ target "_blank"
            , href "https://twitter.com/CedricSoulas"
            ]
            [ text "@CedricSoulas" ]
        ]
    , h2
        [ class "mt-8 max-w-lg" ]
        [ text "Elm Batteries" ]
    , p
        [ class "text-xl" ]
        [ h3 [] [ text "Elm + Parcel + Netlify + Tailwind CSS + Cypress" ]
        , a
            [ href "https://github.com/cedricss/elm-batteries", target "_blank" ]
            [ text "Generate a project ›"
            ]
        ]
    , h2 [ class "mt-4" ] [ text "Elm workshops" ]
    , ul
        [ class "text-xl" ]
        [ li
            []
            [ a
                [ href "https://www.humancoders.com/formations/elm", target "_blank" ]
                [ text "Elm ›" ]
            ]
        , li
            []
            [ a
                [ href "https://www.humancoders.com/formations/elm-avance", target "_blank" ]
                [ text "Elm Advanced ›" ]
            ]
        ]
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        UserClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UserClickedPackageButton ->
            ( { model | package = RemoteData.Loading }
            , Http.get
                { url = "/.netlify/functions/demo"
                , expect =
                    Http.expectJson
                        ServerRespondedWithPackage
                        Data.packageDecoder
                }
            )

        ServerRespondedWithPackage result ->
            ( { model | package = RemoteData.fromResult result }
            , Cmd.none
            )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , package = RemoteData.NotAsked
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
