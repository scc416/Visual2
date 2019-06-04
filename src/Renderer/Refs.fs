(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Refs
    Description: F# references to elements in the DOM + some user settings handling
*)

/// F# References to static parts of renderer DOM
module Refs
open CommonData
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Microsoft.FSharp.Collections
open EEExtensions
open Fable.Import.Electron
open Monaco
open ExecutionTop

// **********************************************************************************
//                                  App Version
// **********************************************************************************

let appVersion = "1.06.9"

// **********************************************************************************
//                               Types used in Renderer
// **********************************************************************************

type VisualEvent =
    | Startup
    | RunningCode

type ErrorT = (int * string)

type LogT = 
    | Wake 
    | Sleep 
    | Step 
    | ParseWithErrors of ErrorT list
    | RunOK 
    | Reset

type LogMessage = {
    LogT: LogT
    Time: int64
    }

/// information stored for each tab(s)
type Editor = { 
    DefaultValue : string
    FileName : string Option
    FilePath : string Option
    IEditor : Monaco.Editor.IEditor option
    Saved : bool
    }

/// Bases to display data in for all Views
/// Udec = unsigned ecimal
type Representations =
    | Hex
    | Bin
    | Dec
    | UDec

/// Select View in RH window
type Views =
    | Registers
    | Memory
    | Symbols

/// user's settings that will be stored on user's computer by electron settings
type VSettings = {
    CurrentFilePath : string
    EditorFontSize : string
    EditorRenderWhitespace : bool
    EditorTheme : string
    EditorWordWrap : bool
    OnlineFetchText : string
    RegisteredKey : string
    SimulatorMaxSteps : string
    }

type Flags = {
    FN : bool
    FZ : bool
    FC : bool
    FV : bool
 }

type DPath = {
    TRegs : uint32 list;
    TFlags : Flags
    }

type TestSetup = {
    Before : DPath;
    Asm : string;
    After : DPath option;
    Name : string
    }



type CodeType = DP3 | DP2 | CMP | LDRSTR | LDMSTM | MISC | EQU | UNIMPLEMENTED

/// Properties for react-tippy
type TooltipsProps =
    | Content of string
    | Animation of string
    | Arrow of bool
    | Theme of string
    | Distance of int
    | HideOnClick of bool
    | Placement of string
    | Delay of int * int

/// Propertis for react-monaco-editor
type EditorsProps =
    | Width of obj
    | Height of obj
    | DefaultValue of obj
    | Language of string
    | Theme of string
    | Options of obj
    | EditorWillMount of (Monaco.IExports -> unit)
    | EditorDidMount of (Monaco.Editor.IEditor -> unit)
    | RequireConfig of obj
    | OnChange of (string -> unit)
    | Value of string
    | IsReadOnly of bool

type TestT = | OkTests | ErrorTests | BetterThanModelTests

/// Position of widget on editor buffer character grid.
/// AboveBelow => offset form position, Exact => centered on position
type WidgetPlace =
    | AboveBelow of HPos : int * VPos : int
    | Exact of HPos : int * VPos : int

type MemDirection = | MemRead | MemWrite

/// type of dialog boxes that comes up in view
type DialogBox =
    | Alert of string
    | OpenFileDl
    | QuitDl
    | ResetEmulatorDl of string * Msg
    | SaveAsDl
    | UnsavedFileDl
    | StepDl
    | StepBackDl
and Msg =
    | ChangeView of Views
    | ChangeRep of Representations
    | ToggleByteView
    | ToggleReverseView
    | NewFile
    | AttemptToDeleteTab of int
    | DeleteTab
    | SelectFileTab of int
    | OpenFile of Editor list
    | OpenFileDialog
    | SaveFile
    | SaveAsFileDialog
    | SaveAsFile of (string * string) option
    | SelectSettingsTab
    | SaveSettings
    | LoadDemoCode
    | IncreaseFontSize
    | DecreaseFontSize
    | CloseDialog
    | AttemptToExit
    | Exit
    | UpdateIEditor of Monaco.Editor.IEditor * int
    | FindEditor
    | FindAndReplaceEditor
    | UndoEditor
    | SelectAllEditor
    | RedoEditor
    | EditorTextChange
    | InitiateClose
    | ReadOnlineInfoSuccess of string * VisualEvent
    | ReadOnlineInfoFail of VisualEvent
    | ResetEmulator
    | InitialiseIExports of Monaco.IExports
    | ReadOnlineInfo of VisualEvent
    | RunSimulation
    | IsItTestbench
    | RunTestBench
    | MatchActiveMode
    | MatchRunMode
    | RunEditorTab of BreakCondition * int64
    | DeleteAllContentWidgets
    | RemoveDecorations
    | RrepareModeForExecution
    | RunEditorRunMode of BreakCondition * int64
    | AsmStepDisplay of BreakCondition * int64 * RunInfo
    | MatchLoadImage of (LoadImage * string list) option * int64 * BreakCondition
    | MatchLoadImageTest of (LoadImage * string list) option * Test list
    | UpdateDialogBox of DialogBox
    | TryParseAndIndentCode of bool * int64 * BreakCondition * Test list option
    | UpdateGUIFromRunState of RunInfo
    | ShowInfoFromCurrentMode
    | HighlightCurrentAndNextIns of string * RunInfo
    | MakeToolTipInfo of int * string * DataPath * ParseTop.CondInstr
    | DisplayState of RunInfo * bool * RunInfo
    | MakeShiftTooltip of int * int * string * DataPath * DataPath * DP.UFlags * RName * DP.ArmShiftType Option * bool * uint32 * DP.Op2
    | MakeEditorInfoButton of string * bool * int * int * string * HTMLElement * string
    | StepCode
    | StepCodeBackBy of int64
    | RunTestBenchOnCode
    | RunEditorTabOnTests of Test list
    | GetTestRunInfo of Result<RunInfo, string> option * bool * Test list
    | PopupMenu of MenuItemOptions List
    | StartTest of Test
    | RunAllEmulatorTests
    | CheckRunMode of Msg * string
    | DialogUpdated

/// the main model for MVU
type Model = { 
    DialogUpdated : bool
    /// File Tab currently selected (and therefore visible)
    TabId : int
    /// tab containing current testbench specification (if testbench is loaded)
    TestbenchTab : int option
    /// Map tabIds to the editors which are contained in them
    Editors : Map<int, Editor>
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
    FlagsHasChanged : bool
    /// Values of all Defined Symols
    SymbolMap : Map<string, uint32 * ExecutionTop.SymbolType>
    /// Current state of simulator
    RunMode : ExecutionTop.RunMode
    /// Global debug level set from main process.
    /// 0 => production. 1 => development. 2 => debug parameter.
    DebugLevel : int
    /// Execution Step number at which GUI was last updated
    LastDisplayStepsDone : int64
    LastOnlineFetchTime : Result<System.DateTime, System.DateTime>
    Activity : bool
    Sleeping : bool
    LastRemindTime : System.TimeSpan option
    Settings : VSettings
    DialogBox : DialogBox option
    InitClose : bool
    Decorations : obj list
    EditorEnable : bool
    ClockTime : uint64 * uint64
    /// Use to set theme for Editors
    IExports : Monaco.IExports option
    }



/// look in the Editors and find the next unique id
let uniqueTabId (editor : Map<int, Editor>) =
    match Map.isEmpty editor with
    | true -> 
        0
    | false -> 
        let lastid, _ = 
            editor
            |> Map.toList
            |> List.rev
            |> List.head
        lastid + 1

/// return the new dialog box when there is no current dialog box
let dialogBoxUpdate newDialogBox =
    function   
    | Option.None -> newDialogBox
    | x -> x 

/// execute the function only if the current tab Id is not -1
/// tabId = -1 means no tab is opened
let executeFunc func =
    function
    | -1 -> ()
    | _ -> func

// ***********************************************************************************************
//                             Top-level Interfaces to Javascript libraries
// ***********************************************************************************************


let initialClose (dispatch : Msg -> unit) =
    function
    | false -> 
        electron.ipcRenderer.on ("closingWindow", (fun event ->
           AttemptToExit |> dispatch
            )) |> ignore
        InitiateClose |> dispatch
    | _ ->
        ()
        
    
//---------------------------------------------SVG------------------------------------------------

// SVG (scalable vector graphics) library is interfaced via built-in FABLE libraries:
// Fable.Helpers.React
// Fable.Helpers.React.Props
// see tooltips.fs for examples and some documentation of SVG helpers

// ***********************************************************************************************
//                              Mini DSL for creating DOM objects
// ***********************************************************************************************

let ELEMENT elName classes (htmlElements : HTMLElement list) =
    let ele = document.createElement elName
    ele.classList.add (classes |> List.toArray)
    List.iter (ele.appendChild >> ignore) htmlElements
    ele

let INNERHTML html (ele : HTMLElement) = (ele.innerHTML <- html); ele
let STYLE (name, value) (ele : HTMLElement) = ele.style.setProperty (name, value); ele

let ID name (ele : HTMLElement) = (ele.id <- name); ele

let DIV = ELEMENT "div"

let TABLE = ELEMENT "table"

let toDOM text = ELEMENT "span" [] [] |> INNERHTML text

let TROW = ELEMENT "tr" []

let TD x = ELEMENT "td" [] <| [ x ]

// *************************************************************************************
//                               References to DOM elements
// *************************************************************************************

/// look up a DOM element
let getHtml = Browser.document.getElementById

// ***********************************************************************************
//                       Functions Relating to Right-hand View Panel
// ***********************************************************************************

/// get id of element containing tab name as dispalyed
let tabNameIdFormatter = sprintf "file-view-name-%d"

// ************************************************************************************
//                         Utility functions used in this module
// ************************************************************************************

[<Emit "'0x' + ($0 >>> 0).toString(16).toUpperCase()">]
let hexFormatter _ : string = jsNative

[<Emit "'u' + ($0 >>> 0).toString(10)">]
let uDecFormatter _ : string = jsNative

// Returns a formatter for the given representation
let formatterWithWidth width rep =
 // TODO: Use binformatter from testformats.fs
    let binFormatter width fmt x =
        let bin a =
            [ 0..width - 1 ]
            |> List.fold (fun s x ->
                match ((a >>> x) % 2u), x with
                | 1u, 7 | 1u, 15 | 1u, 23 -> "_1" + s
                | 0u, 7 | 0u, 15 | 0u, 23 -> "_0" + s
                | 1u, _ -> "1" + s
                | 0u, _ -> "0" + s
                | _ -> failwithf "modulo is broken"
            ) ""
        sprintf fmt (bin x)
    match rep with
    | Hex -> hexFormatter
    | Bin -> (binFormatter width "0b%s")
    | Dec -> (int32 >> sprintf "%d")
    | UDec -> uDecFormatter

let formatter = formatterWithWidth 32

/// Determine whether JS value is undefined
[<Emit("$0 === undefined")>]
let isUndefined (_ : 'a) : bool = jsNative

[<Emit("__dirname")>]

let appDirName : string = jsNative

let initSettings = {
    EditorFontSize = "16"
    SimulatorMaxSteps = "20000"
    EditorTheme = "vs" //"solarised-dark"
    EditorWordWrap = false
    EditorRenderWhitespace = false
    CurrentFilePath = Fable.Import.Node.Exports.os.homedir()
    RegisteredKey = ""
    OnlineFetchText = ""
    }

let themes = [
                "vs", "vs";
                "vs-dark", "vs-dark";
                "hc-black", "hc-black";
                //"one-dark-pro", "One Dark Pro";
                //"one-light-pro", "One Light Pro";
                //"solarised-dark", "Solarised Dark";
                //"solarised-light", "Solarised Light";
              ]

let minFontSize = 6L
let maxFontSize = 60L

let visualDocsPage name =
    match EEExtensions.String.split [| '#' |] name |> Array.toList with
    | [ "" ] -> @"https://tomcl.github.io/visual2.github.io/guide.html#content"
    | [ page ] -> sprintf "https://tomcl.github.io/visual2.github.io/%s.html#content" page
    | [ page; tag ] -> sprintf @"https://tomcl.github.io/visual2.github.io/%s.html#%s" page tag
    | _ -> failwithf "What? Split must return non-empty list!"
    

let initialFlags = { N = false; Z = false; C = false; V = false }

/// Return list of lines in IEditor
let formatText (editor : Monaco.Editor.IEditor option) = 
    editor?getValue ()
    |> (fun (x : string) -> x.Split [| '\n' |])
    |> Array.toList