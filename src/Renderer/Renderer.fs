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
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Views
open Tabs
open MenuBar
open Tooltips
open Files
open Settings
open Editors
open Stats
open Integration
open ExecutionTop
open Core.Option
open Testbench
open Tests

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
    let settings = checkSettings (getJSONSettings initSettings) initSettings
    let m =
        { 
            DialogUpdated = false
            TabInfo = {
                Editors = Map.ofList [ (0, blankTab) ]
                TabId = 0 }
            TestbenchTab = None
            CurrentTabWidgets = Map.empty
            SettingsTab = None
            View = {
                CurrentRep = Hex
                ByteView = false
                ReverseDirection = false
                SymbolMap = Map.empty
                FlagsHaveChanged = false
                CurrentView = Registers
                }
            DisplayedCurrentRep = Hex
            MaxStepsToRun = 50000
            Content = initContent
            RunMode = ExecutionTop.ResetMode
            Activity = true
            Sleeping = false

            Settings = settings
            DialogBox = None
            InitClose = false
            Decorations = []
            EditorEnable = true
            ClockTime = (0uL, 0uL)
            LastDisplayStepsDone = 0L
            IExports = None
            OnlineInfo = {
                LastOnlineFetchTime = Result.Error System.DateTime.Now
                LastRemindTime = None
                DebugLevel = debugLevel
                }

        }
    let cmd = Startup |> ReadOnlineInfo |> Cmd.ofMsg         
    m, cmd

let update (msg : Msg) (m : Model) =
    match msg with
    | DialogUpdated ->
        { m with DialogUpdated = true }, Cmd.none
    | UpdateDialogBox dialogBox ->
        let newDialogBox = dialogBoxUpdate (Some dialogBox) m.DialogBox
        { m with DialogBox = newDialogBox }, Cmd.none
    | ChangeView view -> 
        { m with View = { m.View with CurrentView = view } }, Cmd.none
    | ChangeRep rep ->
        { m with View = { m.View with CurrentRep = rep } }, Cmd.none
    | ToggleByteView -> 
        { m with View = { m.View with ByteView = not m.View.ByteView } }, Cmd.none
    | ToggleReverseView -> 
        { m with View = { m.View with ReverseDirection = not m.View.ReverseDirection } }, Cmd.none
    | NewFile -> 
        let newTabInfo = newFileUpdate m.TabInfo.Editors
        { m with TabInfo = newTabInfo }, Cmd.none
    | EditorTextChange ->
        let i = m.TabInfo
        let newEditor = { i.Editors.[i.TabId] with Saved = false }
        let newEditors = Map.add i.TabId newEditor i.Editors
        { m with TabInfo = { m.TabInfo with Editors = newEditors } }, Cmd.none
    | SelectFileTab id -> 
        let newTabId = selectFileTabUpdate id m.TabInfo.Editors
        { m with TabInfo = { m.TabInfo with TabId = newTabId } }, Cmd.none
    | AttemptToDeleteTab id ->
        let newCmd =
            attemptToDeleteTabUpdate (m.TabInfo, m.DialogBox) 
                                     id
        m, newCmd
    | DeleteTab -> 
        let newInfo, newSettingsTab = 
            deleteTabUpdate (m.TabInfo, m.SettingsTab)
        { m with TabInfo = newInfo
                 SettingsTab = newSettingsTab
                 CurrentTabWidgets = Map.empty }, Cmd.ofMsg CloseDialog
    | OpenFile editors -> 
        let newFilePath, newInfo = 
            openFileUpdate (m.TabInfo, m.Settings.CurrentFilePath)
                           editors
        let newSettings = 
            { m.Settings with CurrentFilePath = newFilePath }
        { m with TabInfo = newInfo
                 Settings = newSettings }, Cmd.ofMsg CloseDialog
    | OpenFileDialog -> 
        let newDialog = dialogBoxUpdate (Some OpenFileDl) m.DialogBox
        { m with DialogBox = newDialog }, Cmd.none
    | SaveFile -> 
        let newInfo, cmd = 
            saveFileUpdate m.TabInfo m.SettingsTab
        { m with TabInfo = newInfo }, cmd
    | SaveAsFileDialog -> 
        let cmd =
            saveAsFileDialogUpdate (m.TabInfo.TabId, m.SettingsTab)
        m, cmd
    | SaveAsFile fileInfo ->
        let newInfo, newFilePathSetting =
            saveAsFileUpdate (m.TabInfo, m.Settings.CurrentFilePath)
                             fileInfo
        let newSettings = 
            { m.Settings with CurrentFilePath = newFilePathSetting }
        { m with TabInfo = newInfo
                 Settings = newSettings }, Cmd.ofMsg CloseDialog
    | SelectSettingsTab ->
        let newInfo =
            selectSettingsTabUpdate (m.SettingsTab, m.TabInfo.Editors)
        { m with TabInfo = newInfo
                 SettingsTab = Some newInfo.TabId }, Cmd.none
    | SaveSettings ->
        let newSettings, newInfo =
            saveSettingsUpdate (m.Settings, m.TabInfo.Editors, m.SettingsTab.Value, m.IExports)
        { m with Settings = newSettings 
                 TabInfo = newInfo
                 SettingsTab = None }, Cmd.none
    | SaveSettingsOnly ->
        let newSettings, newEditors =
            saveSettingsOnlyUpdate (m.Settings, m.IExports, m.SettingsTab.Value, m.TabInfo.Editors )
        let newTabInfo = { m.TabInfo with Editors = newEditors } 
        { m with Settings = newSettings 
                 TabInfo = newTabInfo }, Cmd.none
    | LoadDemoCode -> 
        let newInfo = loadDemo m.TabInfo.Editors
        { m with TabInfo = newInfo }, Cmd.none
    | IncreaseFontSize ->
        let newSettings = 
            { m.Settings with EditorFontSize = string ((int m.Settings.EditorFontSize) + 2) }
        { m with Settings = newSettings }, Cmd.none
    | DecreaseFontSize ->
        let newSettings = 
            { m.Settings with EditorFontSize = string ((int m.Settings.EditorFontSize) - 2) }
        { m with Settings = newSettings }, Cmd.none
    | CloseDialog ->
        { m with DialogBox = None 
                 DialogUpdated = false }, Cmd.none
    | AttemptToExit ->
        let newCmd = attemptToExitUpdate m.TabInfo.Editors
        m, newCmd
    | Exit ->
        close()
        m, Cmd.none
    | UpdateIEditor (x, y) ->
        let newEditors = Map.add y { m.TabInfo.Editors.[y] with IEditor = Some x } m.TabInfo.Editors
        let newInfo = { m.TabInfo with Editors = newEditors }
        { m with TabInfo = newInfo }, Cmd.none
    | FindEditor ->
        let i = m.TabInfo
        let action = i.Editors.[i.TabId].IEditor?getAction ("actions.find")
        action?run ()
        m, Cmd.none
    | FindAndReplaceEditor ->
        let i = m.TabInfo
        let action = i.Editors.[i.TabId].IEditor?getAction ("editor.action.startFindReplaceAction")
        action?run ()
        m, Cmd.none
    | UndoEditor ->
        let i = m.TabInfo
        i.Editors.[i.TabId].IEditor?trigger ("Update.fs", "undo") |> ignore
        m, Cmd.none
    | SelectAllEditor ->
        let i = m.TabInfo
        i.Editors.[i.TabId].IEditor?trigger ("Update.fs", "selectAll") |> ignore
        m, Cmd.none
    | RedoEditor ->
        let i = m.TabInfo
        i.Editors.[i.TabId].IEditor?trigger ("Update.fs", "redo") |> ignore
        m, Cmd.none
    | InitiateClose ->
        { m with InitClose = true }, Cmd.none
    | ReadOnlineInfo ve ->
        let cmd = 
            readOnlineInfo ve
                           m.OnlineInfo
                           m.Settings.OnlineFetchText
        m, cmd
    | ReadOnlineInfoSuccess (newOnlineFetchText, ve) -> 
        let newLastOnlineFetchTime, newLastRemindTime, cmd = 
            readOnlineInfoResultUpdate newOnlineFetchText ve m.OnlineInfo.LastRemindTime true
        let newSettings = { m.Settings with OnlineFetchText = newOnlineFetchText }
        let newOnlineInfo = 
            { m.OnlineInfo with LastOnlineFetchTime = Ok System.DateTime.Now 
                                LastRemindTime = newLastRemindTime }
        { m with OnlineInfo = newOnlineInfo
                 Settings = newSettings }, cmd
    | ReadOnlineInfoFail ve -> 
        let newLastOnlineFetchTime, newLastRemindTime, cmd = 
            readOnlineInfoResultUpdate m.Settings.OnlineFetchText ve m.OnlineInfo.LastRemindTime false
        let newOnlineInfo = 
            { m.OnlineInfo with LastOnlineFetchTime = Ok System.DateTime.Now 
                                LastRemindTime = newLastRemindTime }
        { m with OnlineInfo = newOnlineInfo }, cmd
    | InitialiseIExports iExports -> 
        //iExports?languages?register (registerLanguage)
        //iExports?languages?setMonarchTokensProvider (token)
        { m with IExports = Some iExports }, Cmd.none
    | RunSimulation ->
        let cmd = 
            runSimulation m.TabInfo.TabId
        m, 
        Cmd.batch [ RunningCode |> ReadOnlineInfo |> Cmd.ofMsg
                    cmd ]
    | MatchActiveMode ->
        let runMode, cmd = matchActiveMode m.RunMode
        { m with RunMode = defaultValue m.RunMode runMode }, cmd
    | MatchRunMode ->
        let steps = 
            m.Settings.SimulatorMaxSteps
            |> int64 
            |> stepsFromSettings 
        let cmd = matchRunMode NoBreak steps m.RunMode 
        m, cmd
    | RunEditorTab (bkCon, steps) ->
        m, Cmd.batch [ Cmd.ofMsg PrepareModeForExecution 
                       (bkCon, steps) |> RunEditorRunMode |> Cmd.ofMsg ]
    | RunEditorRunMode (bkCon, steps)->
        let cmd = 
            runEditorRunMode bkCon steps m.RunMode
        m, cmd
    | TryParseAndIndentCode (test, steps, bkCon, testOpt) ->
        let i = m.TabInfo
        let loadImage, newRunMode, newDecorations = 
            tryParseAndIndentCode 
                i.Editors.[i.TabId].IEditor 
                m.OnlineInfo.DebugLevel 
                m.Decorations
        let msg =
            match test with
            | false -> MatchLoadImage (loadImage, steps, bkCon)
            | _ -> MatchLoadImageTest (loadImage, testOpt.Value)
        { m with RunMode = defaultValue m.RunMode newRunMode
                 Decorations = newDecorations }, 
        Cmd.ofMsg msg
    | MatchLoadImage (info, steps, bkCon) ->
        let newEnableEditors, newRunMode, cmd = 
            matchLI m.Content steps bkCon info 
        { m with EditorEnable = defaultValue m.EditorEnable newEnableEditors
                 RunMode = defaultValue m.RunMode newRunMode }, cmd
    | AsmStepDisplay (breakCon, steps, ri) ->
        let newRunMode, cmd =
            asmStepDisplay breakCon m.Settings.SimulatorMaxSteps m.RunMode steps ri m.TabInfo.Editors
        { m with RunMode = newRunMode }, cmd
    | PrepareModeForExecution ->
        let i = m.TabInfo
        let cmd = 
            prepareModeForExecution i.Editors.[i.TabId].IEditor m.RunMode
        m, cmd
    | IsItTestbench ->
        let i = m.TabInfo
        let cmd = isItTestbench i.Editors.[i.TabId].IEditor
        m, cmd
    | ResetEmulator ->
        printfn "Resetting..."
        { m with Content = initContent
                 RunMode = ResetMode
                 ClockTime = (0uL, 0uL)
                 View = { m.View with FlagsHaveChanged = false }
                 EditorEnable = true }, 
        Cmd.batch [ Cmd.ofMsg DeleteAllContentWidgets
                    Cmd.ofMsg RemoveDecorations ]
    | RemoveDecorations ->
        let i = m.TabInfo
        executeFunc 
            (List.iter (fun x -> removeDecorations i.Editors.[i.TabId].IEditor x) m.Decorations)
            i.TabId
        { m with Decorations = [] }, Cmd.none
    | DeleteAllContentWidgets ->
        let i = m.TabInfo
        executeFunc 
            (deleteAllContentWidgets m.CurrentTabWidgets i.Editors.[i.TabId].IEditor)
            i.TabId
        { m with CurrentTabWidgets = Map.empty }, Cmd.none
    | ShowInfoFromCurrentMode ->
        let newSymbolTable, newClkTime, newContent, newFlagsHaveChanged = 
            showInfoFromCurrentMode m.RunMode
        { m with View = 
                    { m.View with SymbolMap = defaultValue m.View.SymbolMap newSymbolTable 
                                  FlagsHaveChanged = defaultValue m.View.FlagsHaveChanged newFlagsHaveChanged }
                 ClockTime = defaultValue m.ClockTime newClkTime 
                 Content = defaultValue m.Content newContent
                  }, 
        Cmd.none
    | UpdateGUIFromRunState ri ->
        let newRunMode, newEditorEnable, cmd = updateGUIFromRunState ri
        { m with RunMode = defaultValue m.RunMode newRunMode
                 EditorEnable = defaultValue m.EditorEnable newEditorEnable
                 }, cmd
    | HighlightCurrentAndNextIns (className, ri) -> 
        let i = m.TabInfo
        let newDecorations, cmd = 
            highlightCurrentAndNextIns className ri i.Editors.[i.TabId].IEditor m.Decorations 
        { m with Decorations = newDecorations }, cmd
    | MakeToolTipInfo (v, orientation, dp, condInstr) ->
        let i = m.TabInfo
        let cmd = 
            toolTipInfo 
                (v, orientation)
                dp
                condInstr
                i.Editors.[i.TabId].IEditor
                m.Settings.EditorTheme
        m, cmd
    | DisplayState (ri', running, ri) ->
        let newLastDisplayStepsDone, cmd =
            displayState ri' running m.TabInfo.TabId ri m.LastDisplayStepsDone m.Settings.SimulatorMaxSteps
        { m with LastDisplayStepsDone = defaultValue m.LastDisplayStepsDone newLastDisplayStepsDone}, 
        cmd
    | MakeShiftTooltip (h, v, orientation, dp, dpAfter, uFAfter, rn, shiftT, alu, shiftAmt, op2) ->
        let cmd = 
            makeShiftTooltip 
                (h, v, orientation)
                (dp, dpAfter,uFAfter)
                rn
                (shiftT, alu)
                shiftAmt
                op2
        m, cmd
    | MakeEditorInfoButton (theme, clickable, h, v, orientation, el, txt) ->
        let i = m.TabInfo
        let newWidgets = 
            makeEditorInfoButtonWithTheme 
                theme
                clickable 
                (h, v, orientation)
                m.CurrentTabWidgets
                i.Editors.[i.TabId].IEditor
                (int m.Settings.EditorFontSize)
                txt
                el
        { m with CurrentTabWidgets = newWidgets }, Cmd.none
    | StepCode ->
        let cmd =
            stepCode m.TabInfo
        m, cmd
    | StepCodeBackBy steps ->
        let i = m.TabInfo
        let newRunMode, editorEnable, cmd = 
            stepCodeBackBy 
                steps 
                m.RunMode
                i.Editors.[i.TabId].IEditor
        { m with RunMode = newRunMode
                 EditorEnable = defaultValue m.EditorEnable editorEnable }, cmd
    | RunTestBenchOnCode ->
        let cmd = 
            runTestbenchOnCode 
                m.TabInfo.Editors
                (Cmd.ofMsg RunTestBench)
        m, cmd
    | RunTestBench ->
        let results, cmdLst, decorations = getParsedTests 0x80000000u m.TabInfo.Editors
        let cmd = runTestbench m.TabInfo.TabId results
        let newDecorations = m.Decorations @ decorations
        let allCmd = 
            Cmd.batch (cmdLst @ [ cmd ])
        { m with Decorations = newDecorations }, allCmd
    | RunEditorTabOnTests lst -> 
        let i = m.TabInfo
        let cmd1 = 
            prepareModeForExecution 
                i.Editors.[i.TabId].IEditor
                m.RunMode
        let cmd2 = runEditorTabOnTests lst m.RunMode
        m, Cmd.batch [ cmd1 ; cmd2 ]
    | GetTestRunInfo (result, startTest, tests) ->
        let runMode, cmd = getTestRunInfoMatch startTest tests result
        { m with RunMode = defaultValue m.RunMode runMode}, cmd
    | MatchLoadImageTest (info, tests) ->
        let editorEnable, resultOpt = getTestRunInfo tests info
        let cmd = (resultOpt, false, tests) |> GetTestRunInfo |> Cmd.ofMsg
        { m with EditorEnable = defaultValue m.EditorEnable editorEnable}, 
        cmd
    | PopupMenu lst ->
        popupMenu lst
        m, Cmd.none
    | StartTest test ->
        let cmd = startTest test m.TabInfo.Editors
        m, cmd
    | RunAllEmulatorTests ->
        runAllEmulatorTests m.Content
        m, Cmd.none
    | CheckRunMode (msg, actionName) ->
        let fMsg = 
            match m.RunMode with
            | ExecutionTop.ResetMode
            | ExecutionTop.ParseErrorMode -> 
                msg
            | _ -> 
                ((sprintf "Can't %s while simulator is running <br> <br>Reset and %s<br>" actionName actionName), msg)
                |> ResetEmulatorDl
                |> UpdateDialogBox 
        m, Cmd.ofMsg fMsg

let view (m : Model) (dispatch : Msg -> unit) =
    initialClose dispatch m.InitClose
    mainMenu m.TabInfo m.OnlineInfo.DebugLevel m.RunMode dispatch
    dialogBox (m.Settings.CurrentFilePath, m.TabInfo, m.SettingsTab, m.DialogBox)
              dispatch
              m.DialogUpdated
    div [ ClassName "window" ] 
        [ header [ ClassName "toolbar toolbar-header" ] 
                 [ div [ ClassName "toolbar-actions" ] 
                       [ div [ ClassName "btn-group" ]
                             [ button [ ClassName "btn btn-default" 
                                        DOMAttr.OnClick (fun _ -> (OpenFileDialog, "open file") |> CheckRunMode |> dispatch) ]
                                      [ span [ ClassName "icon icon-folder" ] [] ]
                               button [ ClassName "btn btn-default"
                                        DOMAttr.OnClick (fun _ -> (SaveFile, "save file") |> CheckRunMode |> dispatch) ]
                                      [ span [ ClassName "icon icon-floppy" ] [] ] ]
                         button [ ClassName "btn btn-fixed btn-default button-run"
                                  DOMAttr.OnClick (fun _ -> RunSimulation |> dispatch) ]
                                [ m.RunMode |> runButtonText |> str ]
                         button [ ClassName "btn btn-default"
                                  DOMAttr.OnClick (fun _ -> ResetEmulator |> dispatch) ]
                                [ str "Reset" ]
                         button [ ClassName "btn btn-default button-back"
                                  DOMAttr.OnClick (fun _ -> StepCodeBackBy 1L |> dispatch) ]
                                [ str " Step" ]
                         button [ ClassName "btn btn-default button-forward"
                                  DOMAttr.OnClick (fun _ -> StepCode |> dispatch ) ]
                                [ str "Step " ]
                         (statusBar m.RunMode)
                         div [ ClassName "btn-group clock" ]
                             [ tooltips (Refs.Content clockSymTooltipStr :: Placement "bottom" :: basicTooltipsPropsLst)
                                        [ button [ ClassName "btn btn-large btn-default clock-symbol" ; Disabled true ]
                                                 [ str "\U0001F551" ]]
                               tooltips (Refs.Content clockTooltipStr :: Placement "bottom" :: basicTooltipsPropsLst)
                                        [ button [ ClassName "btn btn-large btn-default clock-time" ; Disabled true ]
                                                 [ m.ClockTime |> clockText |> str  ] ] ]
                         repButtons m.View.CurrentRep dispatch ] ]
          div [ ClassName "window-content" ] 
              [ div [ ClassName "pane-group" ] 
                    [ div [ ClassName "pane file-view-pane"] 
                          ((editorPanel (m.TabInfo, m.SettingsTab, m.Settings, m.EditorEnable) 
                                       dispatch) @
                           [ div [ m.EditorEnable |> overlayClass |> ClassName ] []])
                      viewPanel (m.View, m.Content, m.RunMode)
                                      dispatch ] ] ]

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReact "app"
|> Program.run