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

let getTBWithTab (editors: Map<int, Editor>) =
    []
    |> List.map (fun tab -> tab, editors.[tab]?getValue ())
    |> List.filter (snd >> String.trim >> String.startsWith "##TESTBENCH")
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
