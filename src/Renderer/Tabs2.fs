(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tabs
    Description: handle editor tabs: each can contain a distinct assembly file
*)

/// implement Monaco editor file tabs

module Tabs2
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open CommonData
open Refs
open Editors

let uniqueTabId (editor : Map<int, Editor>) =
    // Look in fileTabList and find the next unique id
    match Map.isEmpty editor with
    | true -> 0
    | false -> 
        let lastid, _ = 
            editor
            |> Map.toList
            |> List.rev
            |> List.head
        lastid + 1

let selectLastTabId editor =
    editor
    |> Map.toList
    |> List.rev
    |> List.head
    |> fst

let blankTab =
    {
        EditorText = ""
        FileName = None
        FilePath = None
        Saved = true
    }