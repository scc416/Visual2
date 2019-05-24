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
open Tooltips2
open Files2
open Settings2
open Editors2
open Dialog
open Stats2
open Integration

let init _ =
    let debugLevel =
        let argV =
            electron.remote.``process``.argv
            |> Seq.toList
            |> List.tail
            |> List.map (fun s -> s.ToLower())
        let isArg s = List.contains s argV
        if isArg "--debug" || isArg "-d" then 2
        elif isArg "-w" then 1
        else 0
    let settings = checkSettings (getJSONSettings()) initSettings
    let m =
        { 
            CurrentFileTabId = 0
            TestbenchTab = None
            Editors = Map.ofList [ (0, blankTab) ]
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
            DebugLevel = debugLevel
            LastOnlineFetchTime = Result.Error System.DateTime.Now
            Activity = true
            Sleeping = false
            LastRemindTime = None
            Settings = settings
            DialogBox = None
            InitClose = false
            Decorations = []
            EditorEnable = true
            ClockTime = (0uL, 0uL)
        }
    let cmd = 
        readOnlineInfo (m.LastOnlineFetchTime, m.LastRemindTime, m.Settings.OnlineFetchText) 
                       Startup
                                
    m, cmd

let update (msg : Msg) (m : Model) =
    match msg with
    | ChangeView view -> 
        { m with CurrentView = view }, Cmd.none
    | ChangeRep rep ->
        { m with CurrentRep = rep }, Cmd.none
    | ToggleByteView -> 
        { m with ByteView = not m.ByteView }, Cmd.none
    | ToggleReverseView -> 
        { m with ReverseDirection = not m.ReverseDirection }, Cmd.none
    | NewFile -> 
        let newTabId, newEditors = newFileUpdate m.Editors
        { m with CurrentFileTabId = newTabId
                 Editors = newEditors }, Cmd.none
    | EditorTextChange ->
        let newEditor = { m.Editors.[m.CurrentFileTabId] with Saved = false }
        let newEditors = Map.add m.CurrentFileTabId newEditor m.Editors
        { m with Editors = newEditors }, Cmd.none
    | SelectFileTab id -> 
        let newTabId = selectFileTabUpdate id m.Editors
        { m with CurrentFileTabId = newTabId }, Cmd.none
    | AttemptToDeleteTab id ->
        let newDialog, newCmd =
            attemptToDeleteTabUpdate (m.CurrentFileTabId, m.Editors.[m.CurrentFileTabId].Saved, m.DialogBox) 
                                     id
        { m with DialogBox = newDialog }, newCmd
    | DeleteTab -> 
        let newTabId, newEditors, newSettingsTab = 
            deleteTabUpdate (m.CurrentFileTabId, m.Editors, m.SettingsTab)
        { m with CurrentFileTabId = newTabId
                 Editors = newEditors 
                 SettingsTab = newSettingsTab 
                 DialogBox = None 
                 CurrentTabWidgets = Map.empty }, Cmd.none
    | OpenFile editors -> 
        let newEditors, newFilePath, newTabId = 
            openFileUpdate (m.Editors, m.Settings.CurrentFilePath, m.CurrentFileTabId)
                           editors
        let newSettings = 
            { m.Settings with CurrentFilePath = newFilePath }
        { m with Editors = newEditors
                 CurrentFileTabId = newTabId
                 Settings = newSettings
                 DialogBox = None }, Cmd.none
    | OpenFileDialog -> 
        let newDialog = openFileDialogUpdate m.DialogBox
        { m with DialogBox = newDialog }, Cmd.none
    | SaveFile -> 
        let newDialog, newEditors = 
            saveFileUpdate (m.CurrentFileTabId, m.Editors)
        { m with Editors = newEditors
                 DialogBox = newDialog }, Cmd.none
    | SaveAsFileDialog -> 
        let newDialogBox =
            saveAsFileDialogUpdate (m.CurrentFileTabId, m.DialogBox)
        { m with DialogBox = newDialogBox }, Cmd.none
    | SaveAsFile fileInfo ->
        let newEditors, newFilePathSetting =
            saveAsFileUpdate (m.Editors, m.CurrentFileTabId, m.Settings.CurrentFilePath)
                             fileInfo
        let newSettings = 
            { m.Settings with CurrentFilePath = newFilePathSetting }
        { m with DialogBox = None 
                 Editors = newEditors
                 Settings = newSettings }, Cmd.none
    | SelectSettingsTab ->
        let newEditors, newTabId =
            selectSettingsTabUpdate (m.SettingsTab, m.Editors)
        { m with Editors = newEditors
                 CurrentFileTabId = newTabId
                 SettingsTab = Some newTabId }, Cmd.none
    | SaveSettings ->
        let newSettings, newEditors, newId =
            saveSettingsUpdate (m.Settings, m.Editors, m.SettingsTab.Value)
        { m with Settings = newSettings 
                 Editors = newEditors 
                 CurrentFileTabId = newId 
                 SettingsTab = None }, Cmd.none
    | LoadDemoCode -> 
        let newEditors, newId = loadDemo m.Editors
        { m with Editors = newEditors 
                 CurrentFileTabId = newId }, Cmd.none
    | IncreaseFontSize ->
        let newSettings = { m.Settings with EditorFontSize = string ((int m.Settings.EditorFontSize) + 2) }
        { m with Settings = newSettings }, Cmd.none
    | DecreaseFontSize ->
        let newSettings = { m.Settings with EditorFontSize = string ((int m.Settings.EditorFontSize) - 2) }
        { m with Settings = newSettings }, Cmd.none
    | AboutDialog ->
        let newDialog = aboutDialogUpdate m.DialogBox
        { m with DialogBox = newDialog }, Cmd.none
    | CloseDialog ->
        { m with DialogBox = None }, Cmd.none
    | AttemptToExit ->
        let newDialogBox, newCmd = attemptToExitUpdate m.Editors m.DialogBox
        { m with DialogBox = newDialogBox }, newCmd
    | Exit ->
        close()
        { m with DialogBox = None }, Cmd.none
    | UpdateIEditor (x, y) ->
        let newEditors = Map.add y { m.Editors.[y] with IEditor = Some x } m.Editors
        { m with Editors = newEditors }, Cmd.none
    | FindEditor ->
        let action = m.Editors.[m.CurrentFileTabId].IEditor?getAction ("actions.find")
        action?run ()
        m, Cmd.none
    | FindAndReplaceEditor ->
        let action = m.Editors.[m.CurrentFileTabId].IEditor?getAction ("editor.action.startFindReplaceAction")
        action?run ()
        m, Cmd.none
    | UndoEditor ->
        m.Editors.[m.CurrentFileTabId].IEditor?trigger ("Update.fs", "undo") |> ignore
        m, Cmd.none
    | SelectAllEditor ->
        m.Editors.[m.CurrentFileTabId].IEditor?trigger ("Update.fs", "selectAll") |> ignore
        m, Cmd.none
    | RedoEditor ->
        m.Editors.[m.CurrentFileTabId].IEditor?trigger ("Update.fs", "redo") |> ignore
        m, Cmd.none
    | InitiateClose ->
        { m with InitClose = true }, Cmd.none
    | RunSimulation ->
        let cmd = 
            readOnlineInfo (m.LastOnlineFetchTime, m.LastRemindTime, m.Settings.OnlineFetchText)
                           RunningCode
        let newM = runCode ExecutionTop.NoBreak m
        newM, cmd
    | ReadOnlineInfoSuccess (newOnlineFetchText, ve) -> 
        let newLastOnlineFetchTime, newLastRemindTime = 
            readOnlineInfoSuccessUpdate newOnlineFetchText ve m.LastRemindTime
        let newSettings = { m.Settings with OnlineFetchText = newOnlineFetchText }
        { m with LastOnlineFetchTime = Ok System.DateTime.Now 
                 LastRemindTime = newLastRemindTime
                 Settings = newSettings }, Cmd.none
    | ReadOnlineInfoFail ve -> 
        let newLastOnlineFetchTime, newLastRemindTime = 
            readOnlineInfoFailUpdate m.Settings.OnlineFetchText ve m.LastRemindTime
        { m with LastOnlineFetchTime = newLastOnlineFetchTime 
                 LastRemindTime = newLastRemindTime}, Cmd.none
    | UpdateModel m ->
        m, Cmd.none

let view (m : Model) (dispatch : Msg -> unit) =
    initialClose dispatch m.InitClose
    mainMenu dispatch m
    dialogBox (m.DialogBox, m.Settings.CurrentFilePath, m.Editors, m.CurrentFileTabId, m.SettingsTab)
              dispatch
    Browser.console.log(string m.LastOnlineFetchTime)
    div [ ClassName "window" ] 
        [ header [ ClassName "toolbar toolbar-header" ] 
                 [ div [ ClassName "toolbar-actions" ] 
                       [ div [ ClassName "btn-group" ]
                             [ button [ ClassName "btn btn-default" 
                                        DOMAttr.OnClick (fun _ -> OpenFileDialog |> dispatch) ]
                                      [ span [ ClassName "icon icon-folder" ] [] ]
                               button [ ClassName "btn btn-default"
                                        DOMAttr.OnClick (fun _ -> SaveFile |> dispatch) ]
                                      [ span [ ClassName "icon icon-floppy" ] [] ] ]
                         button [ ClassName "btn btn-fixed btn-default button-run"
                                  DOMAttr.OnClick (fun _ -> RunSimulation |> dispatch) ]
                                [ str "Run" ]
                         button [ ClassName "btn btn-default" ]
                                [ str "Reset" ]
                         button [ ClassName "btn btn-default button-back" ]
                                [ str " Step" ]
                         button [ ClassName "btn btn-default button-forward" ]
                                [ str "Step " ]
                         button [ ClassName "btn btn-large btn-default status-bar" ]//; Disabled true ]
                                [ str "-" ]
                         div [ ClassName "btn-group clock" ]
                             [ tooltips (Content clockSymTooltipStr :: Placement "bottom" :: basicTooltipsPropsLst)
                                        [ button [ ClassName "btn btn-large btn-default clock-symbol" ]
                                                 [ str "\U0001F551" ]]
                               tooltips (Content clockTooltipStr :: Placement "bottom" :: basicTooltipsPropsLst)
                                        [ button [ ClassName "btn btn-large btn-default clock-time" ]//; Disabled true ]
                                                 [ str "-" ] ] ]
                         repButtons m.CurrentRep dispatch ] ]
          div [ ClassName "window-content" ] 
              [ div [ ClassName "pane-group" ] 
                    [ div [ ClassName "pane file-view-pane"] 
                          (editorPanel (m.CurrentFileTabId, m.Editors, m.SettingsTab, m.Settings) 
                                       dispatch)
                      div [ ClassName "pane dashboard"
                            dashboardStyle m.CurrentRep ]
                          [ viewButtons m.CurrentView dispatch
                            viewPanel (m.CurrentRep, m.CurrentView, m.RegMap) 
                                      (m.MemoryMap, m.SymbolMap, m.ByteView, m.ReverseDirection)
                                      dispatch
                            footer m.Flags ] ] ] ]

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReact "app"
|> Program.run