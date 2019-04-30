module Renderer

open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

type Model =
    { Count : int
      IsAlertVisible : bool }

type Msg =
    | CounterMsg
    | ChangeAsideTab

let init _ =
        { 
            Count = 42
            IsAlertVisible = true
        }, Cmd.none

let update (msg : Msg) (model : Model) =
    model, Cmd.none

module R = Fable.Helpers.React

let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        R.str "Elmish testing testing2"]

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReact "app"
|> Program.run