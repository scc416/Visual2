module Renderer

open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

type Model =
    { Number : int 
      Initialised : bool }

type Msg =
    | Increase
    | Decrease
    | Reset
    | InitiateClose
    | Exit

let init _ =
    { Number = 0 
      Initialised = false }, Cmd.none

let close() = electron.ipcRenderer.send "doClose" |> ignore

let update (msg : Msg) (model : Model) =
    let m = 
        match msg with
        | Increase -> { model with Number = model.Number + 1 }
        | Decrease -> { model with Number = model.Number - 1 }
        | Reset -> { model with Number = 0 }
        | InitiateClose -> { model with Initialised = true }
        | Exit -> 
            close() 
            model
    m, Cmd.none
    
let initialClose (dispatch : Msg -> unit) =
    function
    | false -> 
        electron.ipcRenderer.on ("closingWindow", (fun event ->
           Exit |> dispatch )) |> ignore
        InitiateClose |> dispatch
    | _ ->
        ()

let view (model : Model) (dispatch : Msg -> unit) =
    initialClose dispatch model.Initialised
    div [ ClassName "window" ] 
        [ header [ ClassName "toolbar toolbar-header" ] 
                 [ div [ ClassName "toolbar-actions" ] 
                       [ button [ ClassName "btn btn-large btn-default"
                                  DOMAttr.OnClick (fun _ -> Increase |> dispatch) ]
                                [ str "+" ]
                         button [ ClassName "btn btn-large btn-default"
                                  DOMAttr.OnClick (fun _ -> Decrease |> dispatch) ]
                                [ str "-" ]
                         button [ ClassName "btn btn-large btn-default" 
                                  DOMAttr.OnClick (fun _ -> Reset |> dispatch) ]
                                [ model.Number |> string |> str ] ] ]
          div [ ClassName "window-content" ] 
              [ div [ ClassName "pane-group" ] 
                    [ div [ ClassName "pane" ] 
                          [ div [ Id "editor" 
                                  Style [ Height "100%"] ] [] ] ] ] ]
Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReact "app"
|> Program.run