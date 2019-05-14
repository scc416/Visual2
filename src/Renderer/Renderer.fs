module Renderer

open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Monaco
open Monaco.Monaco
open MonacoEditor

type Model =
    { 
        Number : int 
        EditorText : string
    }

type Msg =
    | Increase
    | Decrease
    | EditorTextChange of string

let init _ =
        { 
            Number = 0 
            EditorText = "Sample Editor"
        }, Cmd.none

let update (msg : Msg) (model : Model) =
    let m = 
        match msg with
        | Increase -> { model with Number = model.Number + 1 }
        | Decrease -> { model with Number = model.Number - 1 }
        | EditorTextChange text -> { model with EditorText = text }
    m, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ClassName "window" ] 
        [ 
            header [ ClassName "toolbar toolbar-header" ] 
                   [
                       div [ ClassName "toolbar-actions" ] 
                           [
                               button [ 
                                          ClassName "btn btn-large btn-default"
                                          DOMAttr.OnClick (fun _ -> Increase |> dispatch)
                                      ]
                                      [ str "+" ]
                               button [ 
                                          ClassName "btn btn-large btn-default"
                                          DOMAttr.OnClick (fun _ -> Decrease |> dispatch)
                                      ]
                                      [ str "-" ]
                               button [ ClassName "btn btn-large btn-default" ]
                                      [ model.Number |> string |> str ]
                   ]
            ]
            div [ ClassName "window-content" ] 
                [ 
                    div [ ClassName "pane-group" ] 
                        [ 
                             div [ ClassName "pane"] 
                                 [
                                     Editor.editor [
                                                       Editor.OnChange (EditorTextChange >> dispatch) 
                                                       Editor.Value model.EditorText 
                                                   ]
                                 ] 

                                 
                        ] 
                ]
        ]
        
Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReact "app"
|> Program.run