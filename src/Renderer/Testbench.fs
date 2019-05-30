(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Testbench
    Description: File for GUI interface and higher-level automation in testing of assembly programs with
    special testbench files containing sets of tests.
*)

module Testbench

open Fable.Core.JsInterop
open Refs
open EEExtensions
open Helpers
open CommonData
open ExecutionTop
open TestLib
open Elmish

let getTBWithTab (editors: Map<int, Editor>) =
    editors
    |> Map.map (fun _ value -> value.IEditor?getValue ())
    |> Map.filter (fun _ value -> value |> String.trim |> String.startsWith "##TESTBENCH")
    |> Map.toList
    |> function | [ tab, tb ] -> Ok(tab, tb)
                | [] -> Error "No testbench is loaded"
                | _ -> Error "More than one testbench is loaded"

let getTB editors =
    getTBWithTab editors
    |> Result.map snd

let currentTabIsTB tabId (editors : Map<int, Editor>) =
    match tabId with
    | -1 -> false
    | tab -> editors.[tab].IEditor?getValue ()
             |> String.trim |> String.startsWith "##TESTBENCH"

/// Top-level testbench parse. Locate loaded testbench, generate pair of testbench tab ID
/// and Test list, or Error message. If testbench lines contain errors these are highlighted in buffer.
/// Previous error highlights are removed from buffer.
let getParsedTests dStart (editors : Map<int, Editor>) =
    let mutable decorations = []
    let mutable cmd = []
    let processParseErrors (eLst : Result<Test, (int * string) list> list) =
        let highlightErrors tab =
            List.iter (fun (lNum, mess) ->
                printfn "Testbench error %d %s." lNum mess
                decorations <- 
                    Editors.highlightLine editors.[tab].IEditor lNum "editor-line-error" decorations )
        match getTBWithTab editors with
        | Error mess -> Error mess
        | Ok(tab, _) ->
            // delete decorations
            List.iter (Result.mapError (highlightErrors tab) >> ignore) eLst
            match List.errorList eLst with
            | [] -> 
                List.okList eLst |> Ok
            | x ->
                printfn "%A" x
                cmd <- cmd @ [ tab |> SelectFileTab |> Cmd.ofMsg ]
                Error "Parse errors in testbench"

    let initStack = 0xFF000000u
    getTBWithTab editors
    |> Result.bind (
            fun (tab, tb) ->
                String.toUpper tb
                |> String.splitString [| "\n" |]
                |> Array.toList
                |> parseTests initStack dStart
                |> processParseErrors
                |> Result.map (fun x -> tab, x)),
    cmd,
    decorations

let getTestRunInfo (test : Test list) =
    function
    | Some(lim, _) ->
        let dp = initTestDP (lim.Mem, lim.SymInf.SymTab) test.[0]
        match dp with
        | Ok dp -> Some false, getRunInfoFromImageWithInits NoBreak lim dp.Regs dp.Fl Map.empty dp.MM |> Ok |> Some
        | Error e -> Some false, Error e |> Some
    | None -> None, None

let runTests tests =
    match tests with
    | test :: _ ->
        printfn "Running tests"
        (true, 0L, NoBreak, Some tests) |> TryParseAndIndentCode |> Cmd.ofMsg
    | [] -> 
        Cmd.none

let getTestRunInfoMatch startTest tests =
    function
    | Some(Ok ri) ->
        let ri' = { ri with TestState = if startTest then NoTest else Testing tests }
        let steps =
            if startTest then 1L else System.Int64.MaxValue
        ActiveMode(RunState.Running, ri') |> Some,
        (NoBreak, steps, ri')
        |> AsmStepDisplay
        |> Cmd.ofMsg
    | Some(Error eMess) -> 
        None,
        eMess
        |> Alert
        |> UpdateDialogBox
        |> Cmd.ofMsg
    | _ -> 
        None,
        "Can't run tests: current tab must have valid assembler"
        |> Alert
        |> UpdateDialogBox
        |> Cmd.ofMsg

/// Write test Checklines to the buffer containing the testbench file
let writeTest (test : Test) editors =
    editors
    |> getTBWithTab
    |> Result.map (fun (tabId, dat) ->
        dat
        |> String.splitString [| "\n" |]
        |> Array.toList
        |> List.map String.trim
        |> List.chunkAt (String.trim >> String.startsWith "#TEST")
        |> List.collect (fun chunk ->
                    let testLst = String.splitOnWhitespace (List.head chunk) |> Array.toList
                    let testData = List.filter (String.trim >> String.startsWith ">>" >> not) (chunk |> List.tail)
                    match testLst with
                    | "#TEST" :: LITERALNUMB(n, "") :: _ when int n = test.TNum ->
                        (sprintf "#TEST %d" n) :: testData @ test.CheckLines
                    | _ -> chunk) // no change
        |> List.filter ((<>) "")
        |> String.concat "\n"
        |> fun r -> tabId, r)
    |> function | Ok(tabId, txt) ->
                    let editor = editors.[tabId].IEditor
                    editor?setValue txt
                    Cmd.none
                | Error _ -> 
                    "Error! What? can't find testbench to write results!"
                    |> Alert
                    |> UpdateDialogBox
                    |> Cmd.ofMsg

/// Generate one Test of result messages and add them to the testbench buffer.
/// If no errors mark the Test as Passed.
/// test: test to add (one of those in the testbench).
/// dp: DataPath after test simulation ends.
/// Returns true if test has passed.
let addResultsToTestbench (test : Test) (dp : DataPath) editors =
    let goodParse, resultLines = computeTestResults test dp
    writeTest { test with CheckLines = resultLines } editors,
    goodParse

let getTestList editors =
    let result, cmd1, decorations = 
        getParsedTests 0x10000000u editors
    let cmd2, testLst = 
        result
        |> function
            | Error e -> 
                e
                |> Alert
                |> UpdateDialogBox
                |> Cmd.ofMsg,
                []
            | Ok(_, tests) ->
                Cmd.none,
                tests
    let allCmd = Cmd.batch (cmd1 @ [ cmd2 ])
    decorations, allCmd, testLst
