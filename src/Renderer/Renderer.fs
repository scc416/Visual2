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
        /// File Tab currently selected (and therefore visible)
        CurrentFileTabId : int
        /// tab containing current testbench specification (if testbench is loaded)
        TestbenchTab : int option
        /// Map tabIds to the editors which are contained in them
        Editors : Map<int, obj>
        /// Map of content widgets currently on editor, indexed by id
        CurrentTabWidgets : Map<string, obj>
        /// id of tab containing settings form, if this exists
        SettingsTab : int option
        /// The current number representation being used
        CurrentRep : Representations
        /// indicates what the current DOM symbols display representation is
        DisplayedCurrentRep : Representations
        /// The current View in the right-hand pane
        CurrentView : Views
        /// Whether the Memory View is byte of word based
        ByteView : bool
        /// direction of memory addresses
        ReverseDirection : bool
        /// Number of instructions imulated before break. If 0 run forever
        MaxStepsToRun : int
        /// Contents of data memory
        MemoryMap : Map<uint32, uint32>
        /// Contents of CPU registers
        RegMap : Map<CommonData.RName, uint32>
        /// Contents of CPU flags
        Flags : CommonData.Flags
        /// Values of all Defined Symols
        SymbolMap : Map<string, uint32 * ExecutionTop.SymbolType>
        /// version of symbolMap currently displayed
        DisplayedSymbolMap : Map<string, uint32 * ExecutionTop.SymbolType>
        /// Current state of simulator
        RunMode : ExecutionTop.RunMode
        /// Global debug level set from main process.
        /// 0 => production. 1 => development. 2 => debug parameter.
        DebugLevel : int
        LastOnlineFetchTime : Result<System.DateTime, System.DateTime>
        Activity : bool
        Sleeping : bool
        LastRemindTime : System.TimeSpan option
        Settings : VSettings
    }

type Msg =
    | ChangeView of Views
    | ChangeRep of Representations
    | ToggleByteView
    | ToggleReverseView

let init _ =
        { 
            CurrentFileTabId = -1 // By default no tab is open
            TestbenchTab = None
            Editors = Map.empty
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
        | ChangeView view -> { model with CurrentView = view }
        | ChangeRep rep -> { model with CurrentRep = rep }
        | ToggleByteView -> { model with ByteView = not model.ByteView }
        | ToggleReverseView -> { model with ReverseDirection = not model.ReverseDirection }
    m, Cmd.none


let currentRepClassName (rep : Representations) =
    function
    | x when x = rep -> ClassName "btn btn-rep btn-rep-enabled"
    | _ -> ClassName "btn btn-rep"

let currentViewClassName (view : Views)=
    function
    | x when x = view -> ClassName "tab-item active" 
    | _ -> ClassName "tab-item"

let currentTabClassName (view : Views) = 
    function
    | x when x = view  -> ClassName "list-group"
    | _ -> ClassName "list-group invisible"

let byteViewButtonString =
    function
    | false -> str "Enable Byte View"
    | true -> str "Disable Byte View"

let reverseDirectionButtonString =
    function
    | false -> str "Enable Reverse Direction"
    | true -> str "Disable Reverse Direction"

let viewButtonClassName =
    function
    | false -> ClassName "btn full-width btn-byte"
    | true -> ClassName "btn full-width btn-byte btn-byte-active"

let dashboardWidth rep view =
    let w =
        match rep, view with
        | Bin, _ -> "--dashboard-width-binrep"
        | _, Registers -> "--dashboard-width-init-registers"
        | _ -> "--dashboard-width-init-other"
        |> getCustomCSS
    printf "Setting width to %s" w
    w |> setDashboardWidth

let view (model : Model) (dispatch : Msg -> unit) =
    dashboardWidth model.CurrentRep model.CurrentView
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
                                       button [ ClassName "btn btn-large btn-default" ]
                                              [ str "\U0001F551" ]
                                       button [ ClassName "btn btn-large btn-default" ; Disabled true ]
                                              [ str "-" ]
                                   ]
                               div [ ClassName "btn-group pull-right" ] 
                                   [
                                       button [ 
                                                  currentRepClassName Hex model.CurrentRep
                                                  DOMAttr.OnClick (fun _ -> Hex |> ChangeRep |> dispatch)
                                              ]
                                              [ str "Hex" ]
                                       button [ 
                                                  currentRepClassName Bin model.CurrentRep
                                                  DOMAttr.OnClick (fun _ -> Bin |> ChangeRep |> dispatch)
                                              ]
                                              [ str "Bin" ]
                                       button [ 
                                                  currentRepClassName Dec model.CurrentRep
                                                  DOMAttr.OnClick (fun _ -> Dec |> ChangeRep |> dispatch)
                                              ]
                                              [ str "Dec" ]
                                       button [ 
                                                  currentRepClassName UDec model.CurrentRep
                                                  DOMAttr.OnClick (fun _ -> UDec|> ChangeRep |> dispatch)
                                              ]
                                              [ str "uDec" ]
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
                                     div [ Id "controls"]
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
                                                                           currentViewClassName Registers model.CurrentView
                                                                           DOMAttr.OnClick (fun _ -> 
                                                                               Registers |> ChangeView |> dispatch)
                                                                       ] 
                                                                       [ str "Registers" ]
                                                                   div [ 
                                                                           currentViewClassName Memory model.CurrentView
                                                                           DOMAttr.OnClick (fun _ -> 
                                                                               Memory |> ChangeView |> dispatch)
                                                                       ] 
                                                                       [ str "Memory" ]
                                                                   div [ 
                                                                           currentViewClassName Symbols model.CurrentView
                                                                           DOMAttr.OnClick (fun _ -> 
                                                                               Symbols |> ChangeView |> dispatch)
                                                                       ] 
                                                                       [ str "Symbols" ]
                                                               ]
                                                       ]
                                                ]
                                         ]
                                     div [ Id "viewer" ] 
                                         [
                                             ul [ currentTabClassName Registers model.CurrentView ] 
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
                                                                           0u |> formatter model.CurrentRep |> str                                                                           ]
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
                                             ul [ currentTabClassName Memory model.CurrentView ]
                                                [ 
                                                   li [ Class "list-group-item" ]
                                                      [ 
                                                          div [ Class "btn-group full-width" ]
                                                              [
                                                                  button [ 
                                                                             viewButtonClassName model.ByteView
                                                                             DOMAttr.OnClick (fun _ -> ToggleByteView |> dispatch)
                                                                         ]
                                                                         [ byteViewButtonString model.ByteView ]
                                                                  button [ 
                                                                             viewButtonClassName model.ReverseDirection 
                                                                             DOMAttr.OnClick (fun _ -> ToggleReverseView |> dispatch)
                                                                         ]
                                                                         [ reverseDirectionButtonString model.ReverseDirection] 
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
                                                                                  []
                                                                               td [ Class "td-mem" ]
                                                                                  [] 
                                                                           ] 
                                                                    ] 
                                                              ] 
                                                      ]
                                                   li [ ]
                                                      [ 
                                                          div [ Style [ TextAlign "center" ] ]
                                                              [ 
                                                                  b []
                                                                    [ str "Uninitialized memory is zeroed" ] 
                                                              ] 
                                                      ] ]
                                             div [ currentTabClassName Symbols  model.CurrentView ]
                                                 [ table [ ClassName "table-striped" ]
                                                         [] 
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