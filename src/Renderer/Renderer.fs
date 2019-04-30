module Renderer

open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

type Model =
    { 
        Count : int
    }

type Msg =
    | Increase
    | Decrease

let init _ =
        { 
            Count = 0
        }, Cmd.none

let update (msg : Msg) (model : Model) =
    let m = 
        match msg with
        | Increase -> { model with Count = model.Count + 1 }
        | Decrease -> { model with Count = model.Count - 1 }
    m, Cmd.none



let view (model : Model) (dispatch : Msg -> unit) =
    div [ ClassName "window" ] 
        [ 
            header [ ClassName "toolbar toolbar-header" ] 
                   [
                       div [ ClassName "toolbar-actions" ] 
                           [
                               div [ ClassName "btn-group" ] 
                                   [
                                       button [ ClassName "btn btn-default" ]
                                              [
                                                  span [ ClassName "icon icon-folder" ] 
                                                       []
                                              ]
                                       button [ ClassName "btn btn-default" ]
                                              [
                                                  span [ ClassName "icon icon-floppy" ] 
                                                       []
                                              ]

                                   ]
                               button [ ClassName "btn btn-fixed btn-default button-run" ]
                                      [
                                          str "Run"
                                      ]
                               button [ ClassName "btn btn-default" ]
                                      [
                                          str "Reset"
                                      ]
                               button [ 
                                          ClassName "btn btn-default button-back" 
                                          DOMAttr.OnClick (fun _ -> dispatch Decrease)
                                      ]
                                      [
                                          str " Step"
                                      ]
                               button [ 
                                          ClassName "btn btn-default button-forward" 
                                          DOMAttr.OnClick (fun _ -> dispatch Increase)
                                      ]
                                      [
                                          str "Step "
                                      ]
                               button [ ClassName "btn btn-large btn-default" ; Disabled true ]
                                      [
                                          str "-"
                                      ]
                               div [ ClassName "btn-group" ]
                                   [
                                       button [ ClassName "btn btn-large btn-default" ]
                                              [
                                                  str "&x1F551"
                                                  //&x
                                              ]
                                       button [ ClassName "btn btn-large btn-default" ; Disabled true ]
                                              [
                                                  str "-"
                                              ]
                                   ]
                               div [ ClassName "btn-group pull-right" ] 
                                   [
                                       button [ ClassName "btn btn-rep btn-rep-enabled" ]
                                              [
                                                  str "Hex"
                                              ]
                                       button [ ClassName "btn btn-rep" ]
                                              [
                                                  str "Bin"
                                              ]
                                       button [ ClassName "btn btn-rep" ]
                                              [
                                                  str "Dec"
                                              ]
                                       button [ ClassName "btn btn-rep" ]
                                              [
                                                  str "uDec"
                                              ]
                                   ]
                           ]
                   ]
            div [ ClassName "window-content" ] 
                [ 
                    div [ ClassName "pane-group" ] 
                        [ 
                             div [ ClassName "pane"] 
                                 [
                                     div [ 
                                             Id "editor"
                                             Style [ Height "100%" ; Width "100%" ] 
                                         ]
                                         []
                                 ] 
                             div [ ClassName "pane" ; Id "dashboard"] 
                                 [
                                     ul [ ClassName "list-group" ] 
                                        [
                                            li [ 
                                                   ClassName "list-group-item"
                                                   Style [ Padding 0 ]
                                               ] 
                                               [
                                                   div [ ClassName "tab-group full-width" ]
                                                       [
                                                           div [ ClassName "tab-item active" ] 
                                                               [ str "Registers" ]
                                                           div [ ClassName "tab-item" ] 
                                                               [ str "Memory" ]
                                                           div [ ClassName "tab-item" ] 
                                                               [ str "Symbols" ]
                                                       ]
                                               ]
                                        ]
                                     ul [ ClassName "list-group" ] 
                                        [
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R0" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ model.Count |> string |> str ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R1" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R2" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R3" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R4" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R5" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R6" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R7" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R8" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R9" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R10" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R11" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R12" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R13" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R14" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R15" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ str "0x0" ]
                                                       ]
                                               ]
                                        ]
                                     footer [ ClassName "toolbar toolbar-footer" ] 
                                            [
                                                div [ 
                                                        ClassName "pull-right"
                                                        Style [ Margin 5 ] 
                                                    ]
                                                    [
                                                        div [ ClassName "btn-group btn-flag-group" ] 
                                                            [
                                                                button [ ClassName "btn btn-flag" ] 
                                                                       [ str "N" ]
                                                                button [ ClassName "btn btn-flag-con" ] 
                                                                       [ str "0" ]
                                                            ]
                                                        div [ ClassName "btn-group btn-flag-group" ] 
                                                            [
                                                                button [ ClassName "btn btn-flag" ] 
                                                                       [ str "Z" ]
                                                                button [ ClassName "btn btn-flag-con" ] 
                                                                       [ str "0" ]
                                                            ]
                                                        div [ ClassName "btn-group btn-flag-group" ] 
                                                            [
                                                                button [ ClassName "btn btn-flag" ] 
                                                                       [ str "C" ]
                                                                button [ ClassName "btn btn-flag-con" ] 
                                                                       [ str "0" ]
                                                            ]
                                                        div [ ClassName "btn-group btn-flag-group" ] 
                                                            [
                                                                button [ ClassName "btn btn-flag" ] 
                                                                       [ str "V" ]
                                                                button [ ClassName "btn btn-flag-con" ] 
                                                                       [ str "0" ]
                                                            ]
                                                    ]
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