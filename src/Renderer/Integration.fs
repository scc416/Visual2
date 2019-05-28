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

let matchLI =
    function
    | Some(lim, _) -> 
        //disableEditors()
        //let ri = lim |> getRunInfoFromImage breakCondition
        //setCurrentModeActiveFromInfo RunState.Running ri
        //asmStepDisplay breakCondition steps ri
        Cmd.none
    | _ -> Cmd.none

/// Process an editor line parse error. Generate a hover message and line decoration. Set Parse Error Mode
let highlightErrorParse ((err : ParseError), lineNo) opc editor decorations =
    let ML = EEExtensions.String.split [| '\n' |] >> Array.toList
    let codeLines = editor?getValue () |> EEExtensions.String.split [| '\n' |]
    let (gHover, range) =
        if opc <> "" then
            ErrorDocs.getOpcHover "" opc codeLines.[lineNo - 1]
        else ([], (1, 1))
    let link, hover =
        match err with
        | ``Invalid Label`` lab -> "", ML (sprintf "'%s' is not a valid instruction and single character labels are not allowed" lab)
        | ``Invalid syntax`` (wanted, found, page) ->
            page, (ML <| "Parse error\nLooking for: " + wanted) @ (ML <| "Found: " + found)
        | ``Invalid format`` (error, found, page) ->
            page, (ML <| "Format error\n" + error) @ (ML <| "Found: " + found)
        | ``Invalid instruction`` reason ->
            "", ML "This instruction is not valid" @ ML reason
        | ``Label required`` reason ->
            "", ML "This line needs a label" @ ML reason
        | ``Unimplemented parse`` ->
            "", ML "Unimplemented parse: this is an unexpected error, please inform project maintainers"
        | ``Undefined symbol`` syms ->
            let symsMsg =
                match syms with
                | [ sym, msg ] -> sprintf ": %s" msg
                | lst -> List.map snd lst |> String.concat "\n" |> sprintf "s:\n%s"
            "", ML <| "This line contains an expression with undefined symbol" + symsMsg
        | ``Invalid opCode`` (root, cond, suffix) ->
            "", sprintf "This opcode: %A%A%A is not valid" root cond suffix |> ML
        | ``Unimplemented instruction`` opcode when opcode.Length > 1 -> "", sprintf "%s is not a valid UAL instruction" opcode |> ML
        | ``Unimplemented instruction`` opcode -> "", sprintf "%s is not a valid UAL instruction, and one character labels are not allowed" opcode |> ML
        | ``Duplicate symbol`` (sym, lines) ->
            let lineMsg = String.concat ", " (List.map (sprintf "%d") lines)
            "", ML(sprintf "%s: duplicate labels on lines: %s\nDuplicate label names are not allowed" sym lineMsg)
        | ``Literal more than 32 bits`` lit
        | ``Literal is not a valid number`` lit -> "", sprintf "%s is not a valid literal" lit |> ML

    let gLink = []
    let mLink = [ sprintf "[more](%s)" (Refs.visualDocsPage link) ]
    let mHover = hover @ [ "More: see \u26a0" ]
    match err with
    | ``Duplicate symbol`` (sym, lines) -> makeErrorInEditor lineNo hover hover editor decorations
    | _ -> makeErrorInEditor lineNo mHover (gHover @ hover @ mLink @ gLink) editor decorations

    //ParseErrorMode

let imageOfTId = formatText >> reLoadProgram

/// Parse text in tId as program. If parse is OK, indent the program.
/// If parse fails decorate the buffer with error info.
/// Return LoadImage on parse success or None.
let tryParseAndIndentCode editor debugLevel runMode decorations =
    let lim = imageOfTId editor
    let editorASM = lim.EditorText
    // See if any errors exist, if they do display them
    match lim with
    | { Errors = [] } as lim ->
        //Browser.console.log(sprintf "%A" lim)
        let trimmed line = String.trimEnd [| '\r'; '\n' |] line
        let newCode = List.map trimmed lim.Source
        let oldCode = editor |> formatText |> List.map trimmed
        if oldCode <> newCode then
            if debugLevel > 0 then
                if oldCode.Length <> newCode.Length then
                    printfn "Lengths of indented and old code do not match!"
            (editor?setValue (String.concat "\n" newCode)) |> ignore
        (lim, lim.Source) |> Some, runMode, decorations
    | lim ->
        let processParseError (pe, lineNo, opCode) =
            highlightErrorParse (pe, lineNo) opCode editor decorations
        let newDecorations = 
            lim.Errors
            |> List.map processParseError 
            |> List.collect (fun x -> x)
        Core.Option.None, ParseErrorMode, newDecorations

let runEditorRunMode breakCondition steps editor debugLevel runMode decorations =
    match runMode with
    | ResetMode
    | ParseErrorMode _ ->
        let info, newRunMode, newDecorations = 
            tryParseAndIndentCode editor debugLevel runMode decorations
        Cmd.batch [ Cmd.ofMsg RemoveDecorations 
                    newRunMode |> UpdateRunMode |> Cmd.ofMsg
                    newDecorations |> UpdateDecorations |> Cmd.ofMsg 
                    info |> MatchLoadImage |> Cmd.ofMsg ]
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

let currentFileTabProgramIsChanged (pInfo : RunInfo) editor =
    let txt = formatText editor
    let txt' = pInfo.EditorText
    txt.Length <> txt'.Length ||
    List.zip txt txt'
    |> List.exists (fun (a, b) -> invariantOfLine a <> invariantOfLine b)

let prepareModeForExecution runMode editor dialogBox =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode(_, ri) ->
        if currentFileTabProgramIsChanged ri editor 
            then 
                let newDialogBox = AlertVex "Resetting emulator for new execution"
                Cmd.ofMsg ResetEmulator, dialogBoxUpdate newDialogBox dialogBox
            else Cmd.none, dialogBox
    | _ -> Cmd.none, dialogBox

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
    | -1 -> 
        let newDialogBox = AlertVex "No file tab in editor to run!"
        dialogBoxUpdate newDialogBox dialogBox, Cmd.none
    | _ -> dialogBox, Cmd.ofMsg IsItTestbench