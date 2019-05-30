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
    let processParseErrors (eLst : Result<Test, (int * string) list> list) =
        let highlightErrors tab x =
            x
            |> List.map (fun (lNum, mess) ->
                printfn "Testbench error %d %s." lNum mess
                Editors.highlightLine editors.[tab].IEditor lNum "editor-line-error" [] )
            |> List.collect (fun x -> x )
        match getTBWithTab editors with
        | Error mess -> Error mess
        | Ok(tab, _) ->
            //Cmd.ofMsg RemoveEditorDecorations
            List.iter (Result.mapError (highlightErrors tab) >> ignore) eLst
            match List.errorList eLst with
            | [] -> 
                //Cmd.none,
                List.okList eLst |> Ok
            | x ->
                printfn "%A" x
                //tab |> SelectFileTab |> Cmd.ofMsg,
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
                |> Result.map (fun x -> tab, x))

