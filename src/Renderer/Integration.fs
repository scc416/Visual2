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
open Monaco

/// Make map of all data memory locs
let makeDataLocMemoryMap mm =
    Map.toList mm
    |> List.map (fun ((WA addr), value) ->
           match value with
           | Dat x -> Some(addr, x)
           | CodeSpace -> Core.Option.None)
    |> List.choose id
    |> Map.ofList

/// Number of execution steps before checking if button has been pressed
/// and updating displayed state
let maxStepsBeforeCheckEvents : int64 = 5000L
/// Number of execution steps before slowing down display update
let maxStepsBeforeSlowDisplay : int64 = 100000L
/// Number of execution steps before updating displayed state on long runs.
let slowDisplayThreshold : int64 = 20000L

let showInfoFromCurrentMode runMode =
    let isStopped = match runMode with | ActiveMode(Running, _) -> true | _ -> false
    match runMode with
    | FinishedMode ri
    | ActiveMode(_, ri)
    | RunErrorMode ri ->
        let dp, uFl = ri.dpCurrent
        let memoryMap = makeDataLocMemoryMap dp.MM
        let newFlagsHaveChanged = uFl.NZU
        let newContent = 
            {
                RegMap = dp.Regs
                MemoryMap = memoryMap
                Flags = uFl.F
            }
        Some ri.st, 
        Some ((ri.StepsDone |> uint64), (ri.CyclesDone |> uint64)), 
        Some newContent,
        Some newFlagsHaveChanged 
    | _ -> 
        None, None, None, None

/// Apply GUI decorations to instruction line of last PC and current PC.
/// Move current instruction line to middle of window if not visible.
let highlightCurrentAndNextIns classname pInfo editor decorations =
    let (newDecorations : obj list), cmd1 = 
        match pInfo.LastDP with
        | None -> decorations, Cmd.none
        | Some(dp, _uFl) ->
            match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
            | Some(condInstr, lineNo) ->
                let newDecorations = highlightLine editor lineNo classname decorations
                Editors.revealLineInWindow editor lineNo
                newDecorations, (lineNo - 1, "top", dp, condInstr) |> MakeToolTipInfo |> Cmd.ofMsg
            | Option.None
            | Some _ ->
                if dp.Regs.[R15] <> 0xFFFFFFFCu then
                    failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" dp.Regs.[R15]
                else
                    () // special case of return from testbench call
                decorations, Cmd.none
    let pc = (fst pInfo.dpCurrent).Regs.[R15]
    match Map.tryFind (WA pc) pInfo.IMem with
    | Some(condInstr, lineNo) ->
        let newDecorations2 = 
            highlightNextInstruction editor lineNo newDecorations
        let cmd2 = 
            (lineNo - 1, "bottom", fst pInfo.dpCurrent, condInstr) |> MakeToolTipInfo |> Cmd.ofMsg
        let allCmd = Cmd.batch [ cmd1 ; cmd2 ]
        newDecorations2, allCmd
    | _ -> 
        newDecorations, cmd1

let stepCode info : Cmd<Msg> =
    match currentTabIsTB info.TabId info.Editors with
    | false -> 
        (NoBreak, 1L) |> RunEditorTab |> Cmd.ofMsg 
    | true -> 
        "Current file is a testbench: switch to an assembly tab"
        |> Alert 
        |> UpdateDialogBox
        |> Cmd.ofMsg

/// Update GUI after a runtime error or exit. Highlight error line (and make it visible).
/// Set suitable error message hover. Update GUI to 'finished' state on program exit.
/// If running a testbench check results on finish and start next test if passed.
let updateGUIFromRunState (pInfo : RunInfo) : 
                          RunMode option * bool option * Cmd<Msg>=
    let getCodeLineMess pInfo =
        match pInfo.LastDP with
        | None -> ""
        | Some(dp, _) ->
            match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
            | Some(_, lineNo) -> sprintf "on line %d" lineNo
            | _ -> ""
    match pInfo.State with
    | PSError EXIT
    | PSError TBEXIT
    | PSExit ->
        FinishedMode (pInfo) |> Some, 
        Some true, 
        Cmd.batch [ Cmd.ofMsg RemoveDecorations
                    Cmd.ofMsg DeleteAllContentWidgets
                    ("editor-line-highlight", (pInfo)) |> HighlightCurrentAndNextIns |> Cmd.ofMsg
                    Cmd.ofMsg ShowInfoFromCurrentMode ]
    | PSBreak ->
        ActiveMode(Paused, pInfo) |> Some, 
        Some true,
        Cmd.batch [ Cmd.ofMsg RemoveDecorations
                    Cmd.ofMsg DeleteAllContentWidgets
                    ("editor-line-highlight", (pInfo)) |> HighlightCurrentAndNextIns |> Cmd.ofMsg
                    Cmd.ofMsg ShowInfoFromCurrentMode ]
    | PSError(NotInstrMem x) ->
        (RunErrorMode pInfo)|> Some,
        None,
        Cmd.batch [ x |> sprintf "Trying to access non-instruction memory 0x%x" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
                    Cmd.ofMsg ShowInfoFromCurrentMode ]
    | PSError(``Run time error`` (_pos, msg)) ->
        let lineMess = getCodeLineMess pInfo
        let sub dispatch =
            Browser.window.setTimeout (
                msg
                |> (sprintf "Error %s: %s" lineMess )
                |> Alert
                |> UpdateDialogBox, 100, []) |> ignore
                //RunErrorMode pInfo
        (RunMode.RunErrorMode pInfo) |> Some,
        None,
        Cmd.batch [ Cmd.ofMsg RemoveDecorations
                    Cmd.ofMsg DeleteAllContentWidgets
                    Cmd.ofSub sub
                    ("editor-line-highlight-error", (pInfo)) |> HighlightCurrentAndNextIns |> Cmd.ofMsg
                    Cmd.ofMsg ShowInfoFromCurrentMode ]

    | PSError(``Unknown symbol runtime error`` undefs) ->
        RunMode.RunErrorMode pInfo |> Some,
        None,
        Cmd.batch [ undefs |> sprintf "What? Undefined symbols: %A" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
                    Cmd.ofMsg ShowInfoFromCurrentMode ]
    | PSRunning -> 
        failwithf "What? Invalid pInfo.State=PSRunning. Can't update GUI here if still running"
        None,
        None,
        Cmd.ofMsg ShowInfoFromCurrentMode

let handleTest (pInfo : RunInfo) editors =
    match pInfo.TestState, pInfo.State with
    | _, PSExit
    | _, PSError EXIT
    | _, PSError TBEXIT ->
        match pInfo.TestState with
        | NoTest -> 
            printfn "No test!"
            Cmd.none, []
        | Testing [] ->
            Cmd.batch [
                "Bad TestState: Testing []" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
                Cmd.ofMsg ResetEmulator ],
            []
        | Testing(test :: rest) ->
            printfn "Test %d finished!" test.TNum
            let dp = fst pInfo.dpCurrent
            let cmd1, passed = addResultsToTestbench test dp editors
            match passed, rest with
            | true, [] ->
                Cmd.batch 
                    [ "Tests all passed!" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
                      Cmd.ofMsg ResetEmulator ],
                []
            | true, rest -> Cmd.none, rest
            | false, _ ->
                let cmd2 = 
                    Cmd.batch 
                        [ test.TNum |> sprintf "Test %d has errors!" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
                          Cmd.ofMsg ResetEmulator ]
                let cmd3 =   
                    match Testbench.getTBWithTab editors with
                    | Ok(tbTab, _) -> tbTab |> SelectFileTab |> Cmd.ofMsg
                    | _ -> Cmd.none
                Cmd.batch [ cmd1 ; cmd2 ; cmd3 ], []
    | NoTest, _ -> Cmd.none, []
    | _ -> 
        "Test terminated because program has runtime error" |> Alert |> UpdateDialogBox |> Cmd.ofMsg,
        []

/// Main subfunction that updates GUI after a bit of simulation.
/// running: true if trying to run program to end, false if pausing or single stepping.
/// ri': simulator state including whether we have a program end or break or error termination.
let displayState ri' running tabId ri lastDisplayStepsDone simulatorMaxSteps =
    let loopMessage =
        sprintf "WARNING Possible infinite loop: max number of steps (%s) exceeded. 
                 To disable this warning use Edit -> Preferences" 
                simulatorMaxSteps
    match ri'.State with
    | PSRunning -> // simulation stopped without error or exit
        //highlightCurrentAndNextIns "editor-line-highlight" ri' tabId
        let cmd = 
            match running && (int64 simulatorMaxSteps) <> 0L with
            | true -> loopMessage |> Alert |> UpdateDialogBox |> Cmd.ofMsg
            | false -> Cmd.none
        if ri.StepsDone < slowDisplayThreshold || (ri.StepsDone - lastDisplayStepsDone) > maxStepsBeforeSlowDisplay then
            Some ri.StepsDone, 
            Cmd.batch [ cmd
                        Cmd.ofMsg RemoveDecorations
                        Cmd.ofMsg DeleteAllContentWidgets
                        ("editor-line-highlight", ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg 
                        Cmd.ofMsg ShowInfoFromCurrentMode ]
        else None, Cmd.none
    | PSError _
    | PSBreak // execution met a valid break condition
    | PSExit -> // end-of-program termination (from END or implicit drop off code section)
        None, ri' |> UpdateGUIFromRunState |> Cmd.ofMsg

///// Run the simulation from state ri for steps instructions.
///// Steps can be positive or negative, for forward or backward stepping.
///// Keep GUI updated from time to time if steps is large positive.
///// Always update GUI at end.
///// Stored history means that backward stepping will always be fast.
let asmStepDisplay (breakc : BreakCondition) simulatorMaxSteps runMode steps ri' editors =
    let ri = { ri' with BreakCond = breakc }
    match runMode with
    | ActiveMode(Stopping, ri') -> // pause execution from GUI button
        ActiveMode(RunState.Paused, ri'), 
        Cmd.batch [ Cmd.ofMsg ShowInfoFromCurrentMode
                    Cmd.ofMsg RemoveDecorations
                    Cmd.ofMsg DeleteAllContentWidgets
                    ("editor-line-highlight", ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg ]
    | ResetMode -> 
        runMode, Cmd.none // stop everything after reset
    | _ -> // actually do some simulation
        let stepsNeeded = steps - ri.StepsDone // the number of steps still required
        let running = stepsNeeded <> 1L // false if we are single-stepping - a special case
        let stepsMax = maxStepsBeforeCheckEvents // maximum steps to do before updating GUI
        //printfn "exec with steps=%d and R0=%d" ri.StepsDone ri.dpCurrent.Regs.[R0]
        if stepsNeeded <= stepsMax then // in this case we are running to some defined step as part of stepping back, or stepping forward by 1
            let ri' = asmStep steps { ri with BreakCond = NoBreak } // finally run the simulator!
            ActiveMode(Paused, ri'), // mark the fact that we have paused
            Cmd.batch [ (ri', running, ri) |> DisplayState |> Cmd.ofMsg // update GUI
                        Cmd.ofMsg RemoveDecorations
                        Cmd.ofMsg DeleteAllContentWidgets
                        ("editor-line-highlight",  ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg ]
        else // in this case we are running indefinitely, or else for a long time
             // if indefinitely, we could stop on display update timeout, or error, or end of program exit
            let ri' = asmStep (stepsMax + ri.StepsDone - 1L) ri // finally run the simulator!
            let newRMode = ActiveMode(RunState.Running, ri') // mark the fact that we are running
            match ri'.State with
            | PSRunning ->
                let sub dispatch = 
                    Browser.window.setTimeout (
                        // schedule more simulation in the event loop allowing button-press events
                        (ri'.BreakCond, steps, ri') |> AsmStepDisplay |> dispatch, 0, []) |> ignore
                newRMode, Cmd.batch [ Cmd.ofMsg ShowInfoFromCurrentMode
                                      Cmd.ofSub sub ]
            | _ ->
                let cmd, testLst = handleTest ri' editors
                match testLst with
                | [] -> 
                    newRMode, Cmd.batch [ cmd
                                          Cmd.ofMsg ShowInfoFromCurrentMode 
                                          (ri', false, ri) |> DisplayState |> Cmd.ofMsg 
                                          Cmd.ofMsg RemoveDecorations
                                          Cmd.ofMsg DeleteAllContentWidgets
                                          ("editor-line-highlight", ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg ]
                | tests -> 
                    let cmd2 = runTests tests
                    newRMode, Cmd.batch [ cmd
                                          Cmd.ofMsg ShowInfoFromCurrentMode 
                                          (ri', false, ri) |> DisplayState |> Cmd.ofMsg
                                          ("editor-line-highlight", ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg 
                                          cmd2 ]

let getRunInfoFromImage bc (lim : LoadImage) content =
    getRunInfoFromImageWithInits bc lim content.RegMap content.Flags content.MemoryMap lim.Mem

let matchLI content steps breakCon =
    function
    | Some(lim, _) ->
        let ri = getRunInfoFromImage breakCon lim content
        Some false, Some (ActiveMode(RunState.Running, ri)), 
        (breakCon, steps, ri) |> AsmStepDisplay |> Cmd.ofMsg 
    | _ -> 
        None, None, Cmd.none

/// Process an editor line parse error. Generate a hover message and line decoration. Set Parse Error Mode
let highlightErrorParse ((err : ParseError), lineNo) opc (editor : Monaco.Editor.IEditor option) decorations =
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
let tryParseAndIndentCode editor debugLevel decorations =
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
        (lim, lim.Source) |> Some, None, decorations
    | lim ->
        let processParseError (pe, lineNo, opCode) =
            highlightErrorParse (pe, lineNo) opCode editor decorations
        let newDecorations = 
            lim.Errors
            |> List.map processParseError 
            |> List.collect (fun x -> x)
        Core.Option.None, Some ParseErrorMode, newDecorations

let runEditorRunMode breakCondition steps =
    function
    | ResetMode
    | ParseErrorMode _ ->
        Cmd.batch [ Cmd.ofMsg RemoveDecorations 
                    (false, steps, breakCondition, None) |> TryParseAndIndentCode |> Cmd.ofMsg ]
    | ActiveMode(RunState.Paused, ri) ->
        (breakCondition, (steps + ri.StepsDone), ri) |> AsmStepDisplay |> Cmd.ofMsg
    | ActiveMode _
    | RunErrorMode _
    | FinishedMode _ -> 
        Cmd.none

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

let prepareModeForExecution editor =
    function
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode(_, ri) ->
        if currentFileTabProgramIsChanged ri editor 
            then 
                Cmd.batch 
                    [ Cmd.ofMsg ResetEmulator
                      "Resetting emulator for new execution" |> Alert |> UpdateDialogBox |> Cmd.ofMsg ]
            else Cmd.none
    | _ -> Cmd.none

let matchRunMode bkCon steps =
    function
    | FinishedMode _
    | RunErrorMode _ -> 
        Cmd.batch [ Cmd.ofMsg ResetEmulator
                    (bkCon, steps) |> RunEditorTab |> Cmd.ofMsg ]
    | _ -> 
        (bkCon, steps) |> RunEditorTab |> Cmd.ofMsg

let matchActiveMode runMode =
    match runMode with
    | ActiveMode(RunState.Running, ri) -> 
        (RunState.Stopping, ri)|> ActiveMode |> Some,
        Cmd.none
    | _ -> 
        None,
        Cmd.ofMsg MatchRunMode

let isItTestbench editor : Cmd<Msg> =
    let testbench =
        editor?getValue ()
        |> String.trim 
        |> String.startsWith "##TESTBENCH"
    match testbench with
    | true -> Cmd.ofMsg RunTestBench
    | _ -> Cmd.ofMsg MatchActiveMode

/// Step simulation back by numSteps
let stepCodeBackBy numSteps runMode editor : RunMode * bool option * Cmd<Msg> =
    match runMode with
    | ActiveMode(Paused, ri')
    | RunErrorMode ri'
    | FinishedMode ri' ->
        let ri = { ri' with BreakCond = NoBreak }
        if currentFileTabProgramIsChanged ri editor then
            runMode, None, 
            "can't step backwards because execution state is no longer valid"
            |> Alert
            |> UpdateDialogBox
            |> Cmd.ofMsg
        else
            //printf "Stepping back with done=%d  PC=%A" ri.StepsDone ri.dpCurrent
            let target =
                match runMode with
                | RunErrorMode ri -> ri.StepsDone + 1L - numSteps
                | _ -> ri.StepsDone - numSteps
            let runMode = ActiveMode(RunState.Running, ri)
            if target <= 0L then
                runMode,
                None,
                Cmd.batch [
                    Cmd.ofMsg ResetEmulator
                    Cmd.ofMsg RemoveDecorations
                    Cmd.ofMsg ShowInfoFromCurrentMode
                    ]
            else
                printfn "Stepping back to step %d" target
                let ri' = asmStep target ri
                let runMode2 = ActiveMode(RunState.Paused, ri')
                let editorsEnable = Some false
                let cmd = Cmd.ofMsg ShowInfoFromCurrentMode
                let cmd2 = 
                    match ri'.State with
                    | PSRunning ->
                        Cmd.batch 
                            [ Cmd.ofMsg RemoveDecorations
                              Cmd.ofMsg DeleteAllContentWidgets
                              ("editor-line-highlight", ri') |> HighlightCurrentAndNextIns |> Cmd.ofMsg ]
                    | PSError _ | PSExit | PSBreak -> 
                        failwithf "What? Error can't happen when stepping backwards!"
                        Cmd.none
                let allCmd = 
                    [ cmd ]
                    |> List.append [ cmd2 ]
                    |> Cmd.batch
                runMode2, editorsEnable, allCmd

    | ParseErrorMode -> 
        runMode, None, 
        "Can't execute when code has errors"
        |> Alert
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | ResetMode -> 
        runMode, None, 
        "Execution has not started"
        |> Alert
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | _ -> 
        runMode, None, Cmd.none

let runThingOnCode editors cmd : Cmd<Msg> =
    match Testbench.getTBWithTab editors with
    | Ok(tbTab, _) ->
        match editors |> Map.filter (fun id _ -> id <> tbTab) |> Map.toList with
        | [] -> 
            "Can't run Tests because no assembly file is loaded!" 
            |> Alert 
            |> UpdateDialogBox
            |> Cmd.ofMsg
        | [ (id, _) ] ->
            Cmd.batch [ SelectFileTab id |> Cmd.ofMsg ; cmd ]
        | _ -> 
            "Can't run Tests because more than one assembler file is currently loaded. Select the file you wish to test and use Run-> Tests."
            |> Alert 
            |> UpdateDialogBox
            |> Cmd.ofMsg
    | Error e ->
            e
            |> Alert 
            |> UpdateDialogBox
            |> Cmd.ofMsg

let runEditorTabOnTests (tests : Test list) runMode =
    let cmd1 = 
        match tests with
        | [] -> "There are no Tests to run in the testbench!" |> Alert |> UpdateDialogBox |> Cmd.ofMsg
        | _ -> Cmd.none
    let cmd2 = runTests tests
    match runMode with
    | ResetMode
    | ParseErrorMode _ ->
        Cmd.batch 
            [ cmd1 ; Cmd.ofMsg RemoveDecorations ; cmd2 ]
    | ActiveMode _
    | RunErrorMode _
    | FinishedMode _ ->
        Cmd.batch 
            [ cmd1 ; Cmd.ofMsg ResetEmulator ; cmd2 ]

let runTestbench currentTabId (result : Result<(int * Test list), string>) =
    match result with
    | Error(mess) ->
        mess 
        |> Alert
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | Ok(tabId, tests) when currentTabId = tabId ->
        "Please select the program tab which you want to test - not the testbench"
        |> Alert 
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | Ok(_, tests) ->
        printfn "Running %d Tests" tests.Length
        tests |> RunEditorTabOnTests |> Cmd.ofMsg 

let runTestbenchOnCode editors cmd =
    runThingOnCode 
            editors 
            cmd

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runSimulation tabId : Cmd<Msg> =
    match tabId with
    | -1 -> 
        "No file tab in editor to run!"
        |> Alert 
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | _ -> 
        Cmd.ofMsg IsItTestbench

let startTest (test: Test) editors =
    runThingOnCode 
        editors
        (runTests [ test ])