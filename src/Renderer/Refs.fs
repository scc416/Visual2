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
open Node.Exports
open EEExtensions
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

type DialogBox =
    | Alert of string
    | OpenFileDl
    | QuitDl
    | SaveAsDl
    | UnsavedFileDl
    | StepDl
    | StepBackDl

/// Position of widget on editor buffer character grid.
/// AboveBelow => offset form position, Exact => centered on position
type WidgetPlace =
    | AboveBelow of HPos : int * VPos : int
    | Exact of HPos : int * VPos : int

type MemDirection = | MemRead | MemWrite

type Model = { 
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

type Msg =
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
    | SetCurrentModeActive of RunState * RunInfo
    | RunEditorTab of BreakCondition * int64
    | DeleteAllContentWidgets
    | RemoveDecorations
    | RrepareModeForExecution
    | RunEditorRunMode of BreakCondition * int64
    | AsmStepDisplay of BreakCondition * int64 * RunInfo
    | MatchLoadImage of (LoadImage * string list) option * int64 * BreakCondition
    | MatchLoadImageTest of (LoadImage * string list) option
    | UpdateDialogBox of DialogBox
    | TryParseAndIndentCode of bool * int64 * BreakCondition
    | UpdateGUIFromRunState of RunInfo
    | ShowInfoFromCurrentMode
    | HighlightCurrentAndNextIns of string * RunInfo
    | MakeToolTipInfo of int * string * DataPath * ParseTop.CondInstr
    | DisplayState of RunInfo * bool * RunInfo
    | MakeShiftTooltip of int * int * string * DataPath * DataPath * DP.UFlags * RName * DP.ArmShiftType Option * bool * uint32 * DP.Op2
    | MakeEditorInfoButton of string * bool * int * int * string * HTMLElement * string
    | StepCode
    | StepCodeBackBy of int64

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

let dialogBoxUpdate newDialogBox =
    function   
    | Option.None -> newDialogBox
    | x -> x 

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

//------------------------------------------TIPPY.JS----------------------------------------------

/// top-level function from tippy.js to make tooltips
let tippy (rClass : string, tippyOpts : obj) : unit = importDefault "tippy.js"

//-------------------------------------------VEX-JS-----------------------------------------------

/// interfaces to vex.js
[<Emit("require('vex-js');")>]
let vex : obj = jsNative

[<Emit("require('vex-dialog');")>]
let vexDialog : obj = jsNative

vex?defaultOptions?className <- "vex-theme-default"

vex?registerPlugin vexDialog

let vButton (caption : string) =
    createObj [
        "text" ==> caption
        "click" ==> fun () -> "abc"
    ]

let showVexConfirm (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?confirm (createObj [
        "unsafeMessage" ==> htmlMessage;
        "callback" ==> callBack ])
    ()

let showVexPrompt (placeHolder : string) (callBack : string -> unit) (htmlMessage : string) =
    vex?dialog?prompt (createObj [
        "unsafeMessage" ==> htmlMessage
        "placeholder" ==> placeHolder
        "callback" ==> callBack
        ])
    ()

let showVexAlert (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?alert (createObj [ "unsafeMessage" ==> htmlMessage 
                                  "callback" ==> callBack ])
    ()

let validPosInt s =
    match s with
    | null -> Error ""
    | x ->
        printfn "Error %s" x
        x
        |> System.Int32.TryParse
        |> function
            | true, n when n > 0 -> Ok n
            | true, n -> Error "number must be greater than 0"
            | false, _ -> Error "Input a positive integer"

let showVexValidatedPrompt (placeHolder : string) 
                           (validator : string -> Result<'T, string>) 
                           (callBack : 'T -> unit) 
                           (htmlMessage : string) 
                           (callBack2 : Unit) =
    let cb (s : string) =
        match (unbox s) with
        | false -> ()
        | _ ->
            validator s
            |> function
                | Ok valid -> 
                    callBack valid
                    callBack2
                | Error _ -> 
                    callBack2
    showVexPrompt placeHolder cb htmlMessage
    
//---------------------------------------------SVG------------------------------------------------

// SVG (scalable vector graphics) library is interfaced via built-in FABLE libraries:
// Fable.Helpers.React
// Fable.Helpers.React.Props
// see tooltips.fs for examples and some documentation of SVG helpers

// ***********************************************************************************************
//                                  Mini DSL for creating DOM objects
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
            
/// A reference to the settings for the app
/// persistent using electron-settings
let settings : obj = electron.remote.require "electron-settings"

let initSettings = {
    EditorFontSize = "16"
    SimulatorMaxSteps = "20000"
    EditorTheme = "solarised-dark"
    EditorWordWrap = false
    EditorRenderWhitespace = false
    CurrentFilePath = Fable.Import.Node.Exports.os.homedir()
    RegisteredKey = ""
    OnlineFetchText = ""
    }

let themes = [
                "one-dark-pro", "One Dark Pro";
                "one-light-pro", "One Light Pro";
                "solarised-dark", "Solarised Dark";
                "solarised-light", "Solarised Light";
              ]

let minFontSize = 6L
let maxFontSize = 60L

let checkPath (p : string) =
    let p' = path.dirname p
    try
        let stat' = fs.statSync (U2.Case1 p')
        let stat = fs.statSync (U2.Case1 p)
        match (stat.isDirectory(), stat'.isDirectory()) with
        | true, _ -> p
        | false, true -> p'
        | _ -> os.homedir()
    with
        | e -> os.homedir()


let checkSettings (vs : VSettings) vso =
    try
        let checkNum (n : string) (min : int64) (max : int64) (def : string) =
            match int64 n with
            | x when x > max -> def
            | x when x < min -> def
            | x -> x.ToString()
        {
        vs with
            EditorTheme =
                match List.tryFind (fun (th, _) -> (th = vs.EditorTheme)) themes with
                | Some _ -> vs.EditorTheme
                | _ -> printfn "Setting theme to default"
                       vso.EditorTheme
            SimulatorMaxSteps =
                checkNum vs.SimulatorMaxSteps 0L System.Int64.MaxValue vso.SimulatorMaxSteps
            EditorFontSize =
                checkNum vs.EditorFontSize minFontSize maxFontSize vso.EditorFontSize
            CurrentFilePath = checkPath vs.CurrentFilePath
        }
    with
        | _ -> printf "Error parsing stored settings: %A" vs
               vs

let setJSONSettings setting =
    let setSetting (name : string) (value : string) =
        printf "Saving JSON: %A" value
        settings?set (name, value) |> ignore
    printfn "Saving settings to this PC: %A" setting
    setSetting "JSON" (Fable.Import.JS.JSON.stringify setting)


let getJSONSettings initSettings =
    let json = settings?get ("JSON", "undefined")
    printfn "Getting settings"
    match json = "undefined" with
    | true ->
            printfn "No JSON settings found on this PC"
            setJSONSettings()
            initSettings
    | false ->
        try
            let vs = (Fable.Import.JS.JSON.parse json) :?> VSettings
            vs
        with
        | e ->
            printfn "Parse failed: using default settings"
            initSettings

let showMessage1 (callBack : int -> unit) (message : string) (detail : string) (buttons : string list) =
    let rem = electron.remote
    let retFn = unbox callBack
    rem.dialog.showMessageBox (
       (let opts = createEmpty<Fable.Import.Electron.ShowMessageBoxOptions>
        opts.title <- FSharp.Core.Option.None
        opts.message <- message |> Some
        opts.detail <- detail |> Some
        opts.``type`` <- "none" |> Some
        opts.buttons <- buttons |> List.toSeq |> ResizeArray |> Some
        opts), retFn)
    |> ignore

let showAlert (message : string) (detail : string) =
    showVexAlert <|
        sprintf """<p style="text-align:center"><b>%s</b><br>%s</p>""" detail message

let showAlert1 (message : string) (detail : string) =
    let rem = electron.remote
    rem.dialog.showMessageBox (
       (let opts = createEmpty<Fable.Import.Electron.ShowMessageBoxOptions>
        opts.title <- FSharp.Core.Option.None
        opts.message <- message |> Some
        opts.detail <- detail |> Some
        opts.``type`` <- "error" |> Some
        opts))
    |> ignore

let visualDocsPage name =
    match EEExtensions.String.split [| '#' |] name |> Array.toList with
    | [ "" ] -> @"https://tomcl.github.io/visual2.github.io/guide.html#content"
    | [ page ] -> sprintf "https://tomcl.github.io/visual2.github.io/%s.html#content" page
    | [ page; tag ] -> sprintf @"https://tomcl.github.io/visual2.github.io/%s.html#%s" page tag
    | _ -> failwithf "What? Split must return non-empty list!"

/// Run an external URL url in a separate window.
/// Second parameter triggers action (for use in menus)
let runPage url () =
    printf "Running page %s" url
    let rem = electron.remote
    let options = createEmpty<Electron.BrowserWindowOptions>
    // Complete list of window options
    // https://electronjs.org/docs/api/browser-window#new-browserwindowoptions
    options.width <- Some 1200.
    options.height <- Some 800.
    //options.show <- Some false
    let prefs = createEmpty<Electron.WebPreferences>
    prefs.devTools <- Some false
    prefs.nodeIntegration <- Some false
    options.webPreferences <- Some prefs

    options.frame <- Some true
    options.hasShadow <- Some true
    options.backgroundColor <- None
    options.icon <- Some(U2.Case2 "app/visual.ico")
    let window = rem.BrowserWindow.Create(options)
    window.setMenuBarVisibility true
    window.loadURL url
    window.show()

let runExtPage url () =
    electron.shell.openExternal url |> ignore

let writeToFile str path =
    let errorHandler _err = // TODO: figure out how to handle errors which can occur
        ()
    fs.writeFile (path, str, errorHandler)

let initialFlags = { N = false; Z = false; C = false; V = false }

/// Return list of lines in editor
let formatText (editor : Monaco.Editor.IEditor option) =   
    editor?getValue ()
    |> (fun (x : string) -> x.Split [| '\n' |])
    |> Array.toList