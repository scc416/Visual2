(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Integration
    Description: Code to integrate the emulator with the renderer
*)

/// integrate emulator code with renderer
module Integration

open EEExtensions
open Tabs
open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open CommonData
open ParseTop
open ExecutionTop
open Errors
open Refs
open Editors
open TestLib
open Testbench
open Fable.Core.JsInterop
open Fable.Import


let runEditorRunMode breakCondition steps tId editor debugLevel =
    function
    | ResetMode
    | ParseErrorMode _ ->
        //match tryParseAndIndentCode tId editor debugLevel with
        //| Some(lim, _) ->
        //    disableEditors()
        //    let ri = lim |> getRunInfoFromImage breakCondition
        //    setCurrentModeActiveFromInfo RunState.Running ri
        //    asmStepDisplay breakCondition steps ri
        //    Cmd.batch [ Cmd.ofMsg RemoveDecorations ]
        //| _ -> 
            Cmd.ofMsg RemoveDecorations
    | ActiveMode(RunState.Paused, ri) ->
        (breakCondition, (steps + ri.StepsDone), ri) |> AsmStepDisplay |> Cmd.ofMsg   
    | ActiveMode _
    | RunErrorMode _
    | FinishedMode _ -> Cmd.none

let stepsFromSettings =
    function
    | 0L -> System.Int64.MaxValue
    | n when n > 0L -> n
    | _ -> System.Int64.MaxValue

let currentFileTabProgramIsChanged2 (pInfo : RunInfo) editor =
    let txt = formatText editor
    let txt' = pInfo.EditorText
    txt.Length <> txt'.Length ||
    List.zip txt txt'
    |> List.exists (fun (a, b) -> invariantOfLine a <> invariantOfLine b)

let prepareModeForExecution2 runMode editor dialogBox =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode(_, ri) ->
        if currentFileTabProgramIsChanged2 ri editor 
            then Cmd.ofMsg ResetEmulator, dialogBoxUpdate ResettingEmulatorDl dialogBox
            else Cmd.none, None
    | _ -> Cmd.none, None

let matchRunMode =
    function
    | FinishedMode _
    | RunErrorMode _ -> 
        Cmd.batch [ Cmd.ofMsg ResetEmulator
                    Cmd.ofMsg RunEditorTab ]
    | _ -> 
        Cmd.ofMsg RunEditorTab

let matchActiveMode =
    function
    | ActiveMode(RunState.Running, ri) -> 
        (RunState.Running, ri) |> SetCurrentModeActive |> Cmd.ofMsg  
    | _ -> 
        Cmd.ofMsg MatchRunMode

let isItTestbench editor : Cmd<Msg> =
    let testbench =
        editor?getValue ()
        |> String.trim 
        |> String.startsWith "##TESTBENCH"
    match testbench with
    | true -> Cmd.ofMsg RunTestBench
    | _ -> Cmd.ofMsg MatchActiveMode

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runSimulation tabId dialogBox : DialogBox option * Cmd<Msg> =
    match tabId with
    | -1 -> dialogBoxUpdate NoFileTabDl dialogBox, Cmd.none
    | _ -> dialogBox, Cmd.ofMsg IsItTestbench