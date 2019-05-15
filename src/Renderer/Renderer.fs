module Renderer

open Refs
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
open Monaco
open Monaco.Monaco.Languages
open Monaco.Monaco
open Views2
open Tabs2
open MenuBar2
open Update
open Tooltips2

let init _ =
    { 
        CurrentFileTabId = 0
        TestbenchTab = None
        Editors = Map.ofList [ (0, blankTab)]
        CurrentTabWidgets = Map.empty
        SettingsTab = None
        CurrentRep = Hex
        DisplayedCurrentRep = Hex
        CurrentView = Registers
        ByteView = false
        ReverseDirection = false
        MaxStepsToRun = 50000
        MemoryMap = Map.empty
        RegMap = ExecutionTop.initialRegMap
        Flags = initialFlags
        SymbolMap = Map.empty
        DisplayedSymbolMap = Map.empty
        RunMode = ExecutionTop.ResetMode
        DebugLevel = 0
        LastOnlineFetchTime = Result.Error System.DateTime.Now
        Activity = true
        Sleeping = false
        LastRemindTime = None
        Settings = 
            {
                EditorFontSize = "16"
                SimulatorMaxSteps = "20000"
                EditorTheme = "solarised-dark"
                EditorWordWrap = "off"
                EditorRenderWhitespace = "none"
                CurrentFilePath = Fable.Import.Node.Exports.os.homedir()
                RegisteredKey = ""
                OnlineFetchText = ""
            }
    }, Cmd.none

let update (msg : Msg) (model : Model) =
    let m = 
        match msg with
        | ChangeView view -> 
            { model with CurrentView = view }
        | ChangeRep rep ->
            { model with CurrentRep = rep }
        | ToggleByteView -> 
            { model with ByteView = not model.ByteView }
        | ToggleReverseView -> 
            { model with ReverseDirection = not model.ReverseDirection }
        | EditorTextChange str -> 
            let newEditors = editorTextChangeUpdate str 
                                                    model.CurrentFileTabId 
                                                    model.Editors 
            { model with Editors = newEditors }
        | NewFile -> 
            let newTabId = uniqueTabId model.Editors
            let newEditors = Map.add newTabId blankTab model.Editors
            { model with CurrentFileTabId = newTabId
                         Editors = newEditors }
        | SelectFileTab id -> 
            let newTabId = selectFileTabUpdate id model.Editors
            { model with CurrentFileTabId = newTabId }
        | DeleteTab id -> 
            let newTabId, newEditors = deleteTabUpdate id 
                                                       model.CurrentFileTabId 
                                                       model.Editors
            { model with CurrentFileTabId = newTabId
                         Editors = newEditors }
        | OpenFile -> 
            let newEditors, newFilePath, newTabId = 
                openFileUpdate model.Editors 
                               model.Settings.CurrentFilePath
                               model.CurrentFileTabId
            { model with Editors = newEditors
                         CurrentFileTabId = newTabId
                         Settings = { model.Settings with CurrentFilePath = newFilePath }}
    m, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    //mainMenu dispatch
    Browser.console.log(string model.Editors)
    dashboardWidth model.CurrentRep model.CurrentView
    div [ ClassName "window" ] 
        [ 
            header [ ClassName "toolbar toolbar-header" ] 
                   [
                       div [ ClassName "toolbar-actions" ] 
                           [
                               div [ ClassName "btn-group" ]
                                   [
                                       button [
                                                  ClassName "btn btn-default" 
                                                  DOMAttr.OnClick (fun _ -> OpenFile |> dispatch )
                                              ]
                                              [ span [ ClassName "icon icon-folder" ] [] ]
                                       button [ ClassName "btn btn-default" ]
                                              [ span [ ClassName "icon icon-floppy" ] [] ]
                                   ]
                               button [ ClassName "btn btn-fixed btn-default button-run" ]
                                      [ str "Run" ]
                               button [ ClassName "btn btn-default" ]
                                      [ str "Reset" ]
                               button [ ClassName "btn btn-default button-back" ]
                                      [ str " Step" ]
                               button [ ClassName "btn btn-default button-forward" ]
                                      [ str "Step " ]
                               button [ ClassName "btn btn-large btn-default" ; Disabled true ]
                                      [ str "-" ]
                               div [ ClassName "btn-group" ]
                                   [
                                       tooltips (Content clockSymTooltipStr :: 
                                                 Placement "bottom" :: 
                                                 basicTooltipsPropsLst)
                                                [
                                                    button [ ClassName "btn btn-large btn-default" ]
                                                           [ str "\U0001F551" ]
                                                ]
                                       tooltips (Content clockTooltipStr :: 
                                                 Placement "bottom" :: 
                                                 basicTooltipsPropsLst)
                                                [
                                                    button [ ClassName "btn btn-large btn-default" ]//; Disabled true ]
                                                           [ str "-" ]
                                                ]
                                       
                                   ]
                               repButtons model.CurrentRep dispatch
                           ]
                   ]
            div [ ClassName "window-content" ] 
                [ 
                    div [ ClassName "pane-group" ] 
                        [
                             div [ ClassName "pane" ; Id "file-view-pane"] 
                                 (editorPanel model.CurrentFileTabId model.Editors dispatch)
                             div [ ClassName "pane" ; Id "dashboard"] 
                                 [
                                     viewButtons model.CurrentView dispatch
                                     viewPanel model dispatch
                                     footer model.Flags
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