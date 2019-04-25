module Renderer

open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

type CModel =
    { Count : int
      IsAlertVisible : bool }

type PageModel =
    | CounterPage of CModel
    | AlertsPage

type Message =
    | Increase
    | Decrease
    | HideAlert
    | ShowAlert

type ActiveTab =
    | One
    | Two
    | Three

type Model =
    { PageModel : PageModel
      ActiveTab : ActiveTab }

let init2 () =
    { Count = 42
      IsAlertVisible = true }

type Route =
    | Counter
    | Alerts

type Msg =
    | CounterMsg
    | ChangeAsideTab of ActiveTab

let route : Parser<Route -> Route, Route> =
    oneOf
        [ map Counter (s "counter")
          map Alerts (s "alerts") ]

let urlUpdate (result : Route option) model =
    model , Cmd.none

let init result =
    let cm = init2 ()
    let model = { PageModel = CounterPage cm
                  ActiveTab = One }

    urlUpdate result model

let update (msg : Msg) (model : Model) =
    model, Cmd.none

module R = Fable.Helpers.React

let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        R.str "Elmish testing"]

Program.mkProgram init update view
|> Program.toNavigable (parseHash route) urlUpdate
#if DEBUG
|> Program.withHMR
//|> Program.withDebugger
#endif
|> Program.withReact "app"
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.run