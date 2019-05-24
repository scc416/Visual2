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
open Views
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


/// Number of execution steps before checking if button has been pressed
/// and updating displayed state
let maxStepsBeforeCheckEvents : int64 = 5000L

/// Number of execution steps before slowing down display update
let maxStepsBeforeSlowDisplay : int64 = 100000L
/// Number of execution steps before updating displayed state on long runs.
let slowDisplayThreshold : int64 = 20000L
/// URL to display top-level Instruction Help Guide
let guideHTML = "https://tomcl.github.io/visual2.github.io/guide.html"

let fstOf3 (x, _, _) = x
let dpAndInfo (dp, _, dpi) = dp, dpi


/// Process an editor line parse error. Generate a hover message and line decoration. Set Parse Error Mode
let highlightErrorParse ((err : ParseError), lineNo) tId opc (m : Model) =
    let ML = EEExtensions.String.split [| '\n' |] >> Array.toList
    let codeLines = m.Editors.[tId].IEditor?getValue () |> EEExtensions.String.split [| '\n' |]
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
    | ``Duplicate symbol`` (sym, lines) -> makeErrorInEditor tId lineNo hover hover m.Editors
    | _ -> makeErrorInEditor tId lineNo mHover (gHover @ hover @ mLink @ gLink) m.Editors



/// Make map of all data memory locs
let makeDataLocMemoryMap mm =
    Map.toList mm
    |> List.map (fun ((WA addr), value) ->
           match value with
           | Dat x -> Some(addr, x)
           | CodeSpace -> Core.Option.None)
    |> List.choose id
    |> Map.ofList

/// Get simulation data locations from current stored map
let getMemoryMap() : Map<WAddr, MemLoc<CondInstr * int>> =
    memoryMap
    |> Map.toList
    |> List.map (fun (a, v) -> WA a, DataLoc v)
    |> Map.ofList

/// Set current stored register values
let setRegs regs =
    regMap <- regs
    updateRegisters()

/// Get current stored register values
let getRegs() = regMap

/// Set all current stored flags
let setFlags (uFlags : DP.UFlags) =
    let flags = uFlags.F
    setFlag "N" flags.N uFlags.NZU
    setFlag "C" flags.C uFlags.CU
    setFlag "Z" flags.Z uFlags.NZU
    setFlag "V" flags.V uFlags.VU

/// Get all current stored flags
let getFlags() =
    {
        N = getFlag "N"
        C = getFlag "C"
        V = getFlag "V"
        Z = getFlag "Z"
    }

/// Set current stored runMode active
let setCurrentModeActiveFromInfo runState ri =
    setMode (ActiveMode(runState, ri))

let resetEmulator() =
    printfn "Resetting..."
    //Tooltips.deleteAllContentWidgets() INCLUDE THIS
    //let newDecorations = ,///Editors.removeEditorDecorations currentFileTabId
    Editors.enableEditors()
    memoryMap <- Map.empty
    symbolMap <- Map.empty
    regMap <- initialRegMap
    setMode ResetMode
    updateMemory()
    updateSymTable()
    resetRegs()
    resetFlags()
    updateRegisters()
    updateClockTime (0uL, 0uL)

/// Display current execution state in GUI from stored runMode
let showInfoFromCurrentMode (m : Model) =
    let isStopped = match m.RunMode with | ActiveMode(Running, _) -> true | _ -> false
    match m.RunMode with
    | FinishedMode ri
    | ActiveMode(_, ri)
    | RunErrorMode ri ->
        updateSymTable()
        let dp, uFl = ri.dpCurrent
        if currentView = Refs.Views.Memory || isStopped then
            updateMemory()
        setRegs dp.Regs
        setFlags uFl
        updateRegisters()
        updateClockTime ((ri.StepsDone |> uint64), (ri.CyclesDone |> uint64)) |> ignore
        { m with MemoryMap = makeDataLocMemoryMap dp.MM
                 SymbolMap = ri.st
                 RegMap = dp.Regs
                 ClockTime = ((ri.StepsDone |> uint64), (ri.CyclesDone |> uint64))
                 Flags = uFl.F }
    | _ -> m

/// Apply GUI decorations to instruction line of last PC and current PC.
/// Move current instruction line to middle of window if not visible.
let highlightCurrentAndNextIns classname pInfo tId (m : Model) = 
    let m2 = 
        { m with Decorations = removeEditorDecorations tId m.Decorations m.Editors 
                 CurrentTabWidgets = Tooltips.deleteAllContentWidgets m () }
    match pInfo.LastDP with
    | None -> ()
    | Some(dp, _uFl) ->
        match Map.tryFind (WA dp.Regs.[R15]) pInfo.IMem with
        | Some(condInstr, lineNo) ->
            highlightLine tId m.Editors lineNo classname 
            Editors.revealLineInWindow tId lineNo m.Editors 
            Editors.toolTipInfo (lineNo - 1, "top") dp condInstr m 
        | Option.None
        | Some _ ->
            if dp.Regs.[R15] <> 0xFFFFFFFCu then
                failwithf "What? Current PC value (%x) is not an instruction: this should be impossible!" dp.Regs.[R15]
            else
                () // special case of return from testbench call
    let pc = (fst pInfo.dpCurrent).Regs.[R15]
    match Map.tryFind (WA pc) pInfo.IMem with
    | Some(condInstr, lineNo) ->
        highlightNextInstruction lineNo m.Editors.[tId].IEditor 
        Editors.toolTipInfo (lineNo - 1, "bottom") (fst pInfo.dpCurrent) condInstr m
    | _ -> ()


let handleTest (pInfo : RunInfo) =
    match pInfo.TestState, pInfo.State with
    | _, PSExit
    | _, PSError EXIT
    | _, PSError TBEXIT ->
        match pInfo.TestState with
        | NoTest -> printfn "No test!"; []
        | Testing [] ->
            showAlert "Bad TestState: Testing []" "";
            resetEmulator()
            []
        | Testing(test :: rest) ->
            printfn "Test %d finished!" test.TNum
            let dp = fst pInfo.dpCurrent
            let passed = addResultsToTestbench test dp
            match passed, rest with
            | true, [] ->
                showVexAlert "Tests all passed!"
                resetEmulator()
                []
            | true, rest -> rest
            | false, _ ->
                showVexAlert (sprintf "Test %d has errors!" test.TNum)
                resetEmulator()
                match Testbench.getTBWithTab() with
                | Ok(tbTab, _) -> Tabs.selectFileTab tbTab
                | _ -> ()
                []
    | NoTest, _ -> []
    | _ -> showVexAlert "Test terminated because program has runtime error"
           []

/// Update GUI after a runtime error or exit. Highlight error line (and make it visible).
/// Set suitable error message hover. Update GUI to 'finished' state on program exit.
/// If running a testbench check results on finish and start next test if passed.
let UpdateGUIFromRunState(pInfo : RunInfo) (m : Model) =
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
        setMode (FinishedMode pInfo)
        highlightCurrentAndNextIns "editor-line-highlight" (pInfo) currentFileTabId m
        enableEditors()

    | PSBreak ->
        setMode (ActiveMode(Paused, pInfo))
        highlightCurrentAndNextIns "editor-line-highlight" (pInfo) currentFileTabId m
        enableEditors()

    | PSError(NotInstrMem x) ->
        showVexAlert (sprintf "Trying to access non-instruction memory 0x%x" x)
        setMode (RunErrorMode pInfo)

    | PSError(``Run time error`` (_pos, msg)) ->
        let lineMess = getCodeLineMess pInfo
        highlightCurrentAndNextIns "editor-line-highlight-error" pInfo currentFileTabId m
        updateRegisters()
        Browser.window.setTimeout ((fun () ->
            showVexAlert (sprintf "Error %s: %s" lineMess msg)
            RunErrorMode pInfo), 100, []) |> ignore
        setMode (RunMode.RunErrorMode pInfo)

    | PSError(``Unknown symbol runtime error`` undefs) ->
        showVexAlert (sprintf "What? Undefined symbols: %A" undefs)
        setMode (RunMode.RunErrorMode pInfo)
    | PSRunning -> failwithf "What? Invalid pInfo.State=PSRunning. Can't update GUI here if still running"
    showInfoFromCurrentMode m



/// Return executable image of program in editor tab
let imageOfTId tabId editors = 
    editors
    |> formatText tabId
    |> reLoadProgram

/// Return true if program in current editor tab has changed from that in pInfo
let currentFileTabProgramIsChanged (pInfo : RunInfo) tabId (editors : Map<int, Editor>) =
    let txt = formatText tabId editors
    let txt' = pInfo.EditorText
    txt.Length <> txt'.Length ||
    List.zip txt txt'
    |> List.exists (fun (a, b) -> invariantOfLine a <> invariantOfLine b)



/// Parse text in tId as program. If parse is OK, indent the program.
/// If parse fails decorate the buffer with error info.
/// Return LoadImage on parse success or None.
let tryParseAndIndentCode tId (m : Model) =
    let lim = imageOfTId tId m.Editors
    let editorASM = lim.EditorText
    // See if any errors exist, if they do display them
    match lim with
    | { Errors = [] } as lim ->
        //Browser.console.log(sprintf "%A" lim)
        let editor = m.Editors.[tId].IEditor
        let trimmed line = String.trimEnd [| '\r'; '\n' |] line
        let newCode = List.map trimmed lim.Source
        let oldCode = List.map trimmed (formatText tId m.Editors)
        if oldCode <> newCode then
            if m.DebugLevel > 0 then
                if oldCode.Length <> newCode.Length then
                    printfn "Lengths of indented and old code do not match!"
            (editor?setValue (String.concat "\n" newCode)) |> ignore
        (lim, lim.Source) |> Some, m
    | lim ->
        let processParseError (pe, lineNo, opCode) = 
            highlightErrorParse (pe, lineNo) tId opCode
        let error = List.map processParseError lim.Errors |> ignore
        Core.Option.None, { m with RunMode = ParseErrorMode }



let getRunInfoFromImage bc (m : Model) (lim : LoadImage) =
    getRunInfoFromImageWithInits bc lim (m.RegMap) (m.Flags) m.MemoryMap lim.Mem 


/// Execution Step number at which GUI was last updated
let mutable lastDisplayStepsDone = 0L

let getTestRunInfo test codeTid breakCond m =
    let loadIm, m2 = tryParseAndIndentCode codeTid m
    match loadIm with
    | Some(lim, _) ->
        let dp = initTestDP (lim.Mem, lim.SymInf.SymTab) test
        Editors.disableEditors()
        match dp with
        | Ok dp -> getRunInfoFromImageWithInits breakCond lim dp.Regs dp.Fl Map.empty dp.MM |> Ok |> Some
        | Error e -> Error e |> Some
    | None -> None

let runThingOnCode thing =
    match Testbench.getTBWithTab() with
    | Ok(tbTab, _) ->
        match fileTabList |> List.filter (fun id -> id <> tbTab) with
        | [] -> showVexAlert "Can't run Tests because no assembly file is loaded!"
        | [ id ] ->
            Tabs.selectFileTab id
            thing()
        | _ -> showVexAlert "Can't run Tests because more than one assembler file is currently loaded. Select the file you wish to test and use Run-> Tests."
    | Error e -> showVexAlert e


let runTests startTest tests stepFunc m =
    let codeTid = Refs.currentFileTabId
    match tests with
    | test :: _ ->
        printfn "Running tests"
        match getTestRunInfo test codeTid NoBreak m with
        | Some(Ok ri) ->
            let ri' = { ri with TestState = if startTest then NoTest else Testing tests }
            setCurrentModeActiveFromInfo RunState.Running ri'
            stepFunc (if startTest then 1L else System.Int64.MaxValue) ri'
        | Some(Error eMess) -> showVexAlert eMess
        | _ -> showVexAlert "Can't run tests: current tab must have valid assembler"
    | [] -> ()


/// Run the simulation from state ri for steps instructions.
/// Steps can be positive or negative, for forward or backward stepping.
/// Keep GUI updated from time to time if steps is large positive.
/// Always update GUI at end.
/// Stored history means that backward stepping will always be fast.
let rec asmStepDisplay (breakc : BreakCondition) (m : Model) steps ri'  =
    let ri = { ri' with BreakCond = breakc }
    let loopMessage() =
        let steps = m.Settings.SimulatorMaxSteps
        sprintf "WARNING Possible infinite loop: max number of steps (%s) exceeded. To disable this warning use Edit -> Preferences" steps
    /// Main subfunction that updates GUI after a bit of simulation.
    /// running: true if trying to run program to end, false if pausing or single stepping.
    /// ri': simulator state including whether we have a program end or break or error termination.
    let displayState ri' running =
            match ri'.State with
            | PSRunning -> // simulation stopped without error or exit
                highlightCurrentAndNextIns "editor-line-highlight" ri' m.CurrentFileTabId m 
                let m2 = showInfoFromCurrentMode m
                let m3 = 
                    if ri.StepsDone < slowDisplayThreshold || (ri.StepsDone - lastDisplayStepsDone) > maxStepsBeforeSlowDisplay then
                        lastDisplayStepsDone <- ri.StepsDone
                        showInfoFromCurrentMode m2 
                        else m2
                if running && (int64 m.Settings.SimulatorMaxSteps) <> 0L then showVexAlert (loopMessage())
                m3
            | PSError e -> // Something went wrong causing a run-time error
                UpdateGUIFromRunState ri' m//TODO::*3
            | PSBreak // execution met a valid break condition
            | PSExit -> // end-of-program termination (from END or implicit drop off code section)
                UpdateGUIFromRunState ri' m

    match runMode with
    | ActiveMode(Stopping, ri') -> // pause execution from GUI button
        setCurrentModeActiveFromInfo RunState.Paused ri' 
        let m4 = showInfoFromCurrentMode m
        highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId m
    | ResetMode -> () // stop everything after reset
    | _ -> // actually do some simulation
        let stepsNeeded = steps - ri.StepsDone // the number of steps still required
        let running = stepsNeeded <> 1L // false if we are single-stepping - a special case
        let stepsMax = maxStepsBeforeCheckEvents // maximum steps to do before updating GUI
        //printfn "exec with steps=%d and R0=%d" ri.StepsDone ri.dpCurrent.Regs.[R0]
        if stepsNeeded <= stepsMax then // in this case we are running to some defined step as part of stepping back, or stepping forward by 1
            let ri' = asmStep steps { ri with BreakCond = NoBreak } // finally run the simulator!
            setCurrentModeActiveFromInfo Paused ri' // mark the fact that we have paused
            let m4 = displayState ri' running // update GUI
            highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId m
        else // in this case we are running indefinitely, or else for a long time
             // if indefinitely, we could stop on display update timeout, or error, or end of program exit
            let ri' = asmStep (stepsMax + ri.StepsDone - 1L) ri // finally run the simulator!
            setCurrentModeActiveFromInfo RunState.Running ri' // mark the fact that we are running
            let m4 = showInfoFromCurrentMode m // main function to update GUI
            match ri'.State with
            | PSRunning -> //
                 Browser.window.setTimeout ((fun () ->
                        // schedule more simulation in the event loop allowing button-press events
                        asmStepDisplay ri'.BreakCond m steps ri'), 0, []) |> ignore
            | _ ->
                let m5 = displayState ri' false // update GUI
                highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId m
                match handleTest ri' with
                | [] -> ()
                | tests -> runTests false tests (asmStepDisplay NoBreak m) m



/// If program has changed reset execution
let prepareModeForExecution (m : Model) : Model =
    match runMode with
    | FinishedMode ri
    | RunErrorMode ri
    | ActiveMode(_, ri) ->
        if currentFileTabProgramIsChanged ri m.CurrentFileTabId m.Editors then
            showVexAlert "Resetting emulator for new execution" |> ignore
            resetEmulator()
            { m with MemoryMap = Map.empty
                     SymbolMap = Map.empty
                     RegMap = initialRegMap
                     RunMode = ResetMode
                     ClockTime = (0uL, 0uL) }
            else m
    | _ -> m

/// Parses and runs the assembler program in the current tab
/// Aborts after steps instructions, unless steps is 0, or
/// if breackCondition happens
let runEditorTab breakCondition (m : Model) steps : Model =
    match m.CurrentFileTabId with
    | -1 ->
        showVexAlert "No file tab in editor to run!"
        m
    | _ ->
        let m2 = prepareModeForExecution m
        match m2.RunMode with
        | ResetMode
        | ParseErrorMode _ ->
            let tId = m2.CurrentFileTabId
            let decorations2 = removeEditorDecorations tId m.Decorations m2.Editors 
            let m3 = { m2 with Decorations = decorations2 }
            let limStr, m4 = tryParseAndIndentCode tId m
            match limStr with 
            | Some(lim, _) ->
                //TODO: disableEditors()//TODO:
                let ri = lim |> getRunInfoFromImage breakCondition m
                let m5 = { m4 with RunMode = ActiveMode(RunState.Running, ri) }
                asmStepDisplay breakCondition m5 steps ri //TODO::*3
                m5
            | _ -> m4
        | ActiveMode(RunState.Paused, ri) ->
            asmStepDisplay breakCondition m (steps + ri.StepsDone) ri
            m2
        | ActiveMode _
        | RunErrorMode _
        | FinishedMode _ -> 
            m2



/// Step simulation forward by 1
let stepCode tabId editors (m : Model) : Model =
    match currentTabIsTB tabId editors with
    | false -> runEditorTab NoBreak m 1L 
    | true -> 
        showVexAlert "Current file is a testbench: switch to an assembly tab"
        m

/// Step simulation back by numSteps
let stepCodeBackBy (m : Model) numSteps =
    match m.RunMode with
    | ActiveMode(Paused, ri')
    | RunErrorMode ri'
    | FinishedMode ri' ->
        let ri = { ri' with BreakCond = NoBreak }
        if currentFileTabProgramIsChanged ri m.CurrentFileTabId m.Editors then
            showVexAlert "can't step backwards because execution state is no longer valid"
            m
        else
            //printf "Stepping back with done=%d  PC=%A" ri.StepsDone ri.dpCurrent
            let target =
                match runMode with
                | RunErrorMode ri -> ri.StepsDone + 1L - numSteps
                | _ -> ri.StepsDone - numSteps
            setCurrentModeActiveFromInfo RunState.Running ri

            if target <= 0L then
                resetEmulator()
                let newDecorations = 
                    removeEditorDecorations currentFileTabId m.Decorations m.Editors
                showInfoFromCurrentMode m
            else
                printfn "Stepping back to step %d" target
                setCurrentModeActiveFromInfo RunState.Paused ri
                let ri' = asmStep target ri
                setCurrentModeActiveFromInfo Paused ri'
                //TODO: disableEditors()
                match ri'.State with
                | PSRunning ->
                    highlightCurrentAndNextIns "editor-line-highlight" ri' currentFileTabId m
                | PSError _ | PSExit | PSBreak -> failwithf "What? Error can't happen when stepping backwards!"
                showInfoFromCurrentMode m
    | ParseErrorMode -> 
        showVexAlert (sprintf "Can't execute when code has errors")
        m
    | ResetMode -> 
        showVexAlert (sprintf "Execution has not started")
        m
    | _ ->
        m

/// Step simulation back by 1 instruction
let stepCodeBack (m : Model) = stepCodeBackBy m 1L


let runEditorTabOnTests (tests : Test list) (m : Model) =
        if tests = [] then showVexAlert "There are no Tests to run in the testbench!"
        let runT() = runTests false tests (asmStepDisplay NoBreak m) m
        let m2 = prepareModeForExecution m
        match m2.RunMode with
        | ResetMode
        | ParseErrorMode _ ->
            let tId = m.CurrentFileTabId
            let newDecorations = Editors.removeEditorDecorations tId m.Decorations m.Editors
            runT()
        | ActiveMode _
        | RunErrorMode _
        | FinishedMode _ ->
            resetEmulator();
            runT()

let runTestbench (m : Model) =
    match getParsedTests 0x80000000u m with
    | Error(mess) ->
        showVexAlert mess
    | Ok(tabId, tests) when Refs.currentFileTabId = tabId ->
        showVexAlert "Please select the program tab which you want to test - not the testbench"
    | Ok(_, tests) ->
        printfn "Running %d Tests" tests.Length
        runEditorTabOnTests tests m

let runTestbenchOnCode m =
    runThingOnCode (fun () -> runTestbench m)


let startTest test (m : Model) =
    runThingOnCode (fun () -> runTests true [ test ] (asmStepDisplay NoBreak m) m) 

/// Top-level simulation execute
/// If current tab is TB run TB if this is possible
let runCode breakCondition (m : Model) : Model =
    match currentTabIsTB m.CurrentFileTabId m.Editors with
    | true -> m//TODO: runTestbenchOnCode()
    | false ->
        match m.RunMode with
        | ActiveMode(RunState.Running, ri) -> 
            { m with RunMode = ActiveMode((RunState.Stopping), ri) }
        | _ ->
            let m2 = 
                match m.RunMode with
                | FinishedMode _
                | RunErrorMode _ -> 
                    let newDecorations = 
                        removeEditorDecorations m.CurrentFileTabId m.Decorations m.Editors
                    let newCurrentWidgets =
                        Tooltips.deleteAllContentWidgets m
                    { m with MemoryMap = Map.empty
                                         SymbolMap = Map.empty
                                         RegMap = initialRegMap
                                         RunMode = ResetMode
                                         ClockTime = (0uL, 0uL)
                                         Decorations = newDecorations
                                         CurrentTabWidgets = newCurrentWidgets() }//TODO: resetEmulator()
                | _ -> m
            runEditorTab breakCondition m2 <| //TODO: **1
                match int64 m.Settings.SimulatorMaxSteps with
                | 0L -> System.Int64.MaxValue
                | n when n > 0L -> n
                | _ -> System.Int64.MaxValue