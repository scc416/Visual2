module Renderer

open Refs
open Views
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
        CurrentView : Views
        CurrentRep : Representations
    }

type Msg =
    | ChangeView of Views
    | ChangeRep of Representations

let init _ =
        { 
            CurrentView = Registers
            CurrentRep = Hex
        }, Cmd.none

let update (msg : Msg) (model : Model) =
    let m = 
        match msg with
        | ChangeView view -> { model with CurrentView = view }
        | ChangeRep rep -> { model with CurrentRep = rep }
    m, Cmd.none


let currentRepClassName =
    function
    | true -> ClassName "btn btn-rep btn-rep-enabled"
    | _ -> ClassName "btn btn-rep"

let currentViewClassName =
    function
    | true -> ClassName "tab-item active" 
    | _ -> ClassName "tab-item"

let currentTabClassName = 
    function
    | true -> ClassName "list-group"
    | _ -> ClassName "list-group invisible"

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
                               button [ ClassName "btn btn-default button-back" ]
                                      [
                                          str " Step"
                                      ]
                               button [ ClassName "btn btn-default button-forward" ]
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
                                       button [ 
                                                  currentRepClassName (Hex = model.CurrentRep)
                                                  DOMAttr.OnClick (fun _ -> Hex |> ChangeRep |> dispatch)
                                              ]
                                              [
                                                  str "Hex"
                                              ]
                                       button [ 
                                                  currentRepClassName (Bin = model.CurrentRep)
                                                  DOMAttr.OnClick (fun _ -> Bin |> ChangeRep |> dispatch)
                                              ]
                                              [
                                                  str "Bin"
                                              ]
                                       button [ 
                                                  currentRepClassName (Dec = model.CurrentRep)
                                                  DOMAttr.OnClick (fun _ -> Dec |> ChangeRep |> dispatch)
                                              ]
                                              [
                                                  str "Dec"
                                              ]
                                       button [ 

                                                  DOMAttr.OnClick (fun _ -> UDec|> ChangeRep |> dispatch)
                                                  currentRepClassName (UDec = model.CurrentRep)
                                              ]
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
                                                           div [ 
                                                                   currentViewClassName (Registers = model.CurrentView)
                                                                   DOMAttr.OnClick (fun _ -> 
                                                                       Registers |> ChangeView |> dispatch)
                                                               ] 
                                                               [ str "Registers" ]
                                                           div [ 
                                                                   currentViewClassName (Memory = model.CurrentView)
                                                                   DOMAttr.OnClick (fun _ -> 
                                                                       Memory |> ChangeView |> dispatch)
                                                               ] 
                                                               [ str "Memory" ]
                                                           div [ 
                                                                   currentViewClassName (Symbols = model.CurrentView)
                                                                   DOMAttr.OnClick (fun _ -> 
                                                                       Symbols |> ChangeView |> dispatch)
                                                               ] 
                                                               [ str "Symbols" ]
                                                       ]
                                               ]
                                        ]
                                     ul [ currentTabClassName (Registers = model.CurrentView) ] 
                                        [
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R0" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R1" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R2" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R3" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R4" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R5" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R6" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R7" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R8" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R9" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R10" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R11" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R12" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R13" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R14" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                            li [ ClassName "list-group-item" ] 
                                               [
                                                   div [ ClassName "btn-group full-width" ] 
                                                       [
                                                           button [ ClassName "btn btn-reg" ] 
                                                                  [ str "R15" ]
                                                           span [ ClassName "btn btn-reg-con selectable-text" ] 
                                                                [ 
                                                                    0u |> formatter model.CurrentRep |> str  
                                                                ]
                                                       ]
                                               ]
                                        ]
                                     ul [ currentTabClassName (Memory = model.CurrentView) ]
                                        [ 
                                           li [ Class "list-group-item" ]
                                              [ 
                                                  div [ Class "btn-group full-width" ]
                                                      [
                                                          button [ Class "btn full-width btn-byte" ]
                                                                 [ str "Enable Byte View" ]
                                                          button [ Class "btn full-width btn-byte" ]
                                                                 [ str "Reverse Direction" ] 
                                                      ] 
                                              ]
                                           li [ Class "list-group" ]
                                              [
                                                  div [ 
                                                          Class "list-group-item"
                                                          Style [ Padding "0px" ] 
                                                      ]
                                                      [ 
                                                      table [ Class "table-striped" ]
                                                            [ 
                                                                tr [ Class "tr-head-mem" ]
                                                                   [ 
                                                                       th [ Class "th-mem" ]
                                                                          [ str "Address" ]
                                                                       th [ Class "th-mem" ]
                                                                          [ str "Value" ] 
                                                                   ]
                                                                tr [ ]
                                                                   [ 
                                                                       td [ Class "td-mem" ]
                                                                          [ ]
                                                                       td [ Class "td-mem" ]
                                                                          [ ] 
                                                                   ] 
                                                            ] 
                                                      ] 
                                              ]
                                           li [ ]
                                             [ div [ Style [ TextAlign "center" ] ]
                                                 [ b []
                                                     [ str "Uninitialized memory is zeroed" ] ] ] ]
                                     div [ currentTabClassName (Symbols = model.CurrentView) ]
                                         [ table [ ClassName "table-striped" ]
                                                 [] 
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