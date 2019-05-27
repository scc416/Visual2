(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tabs
    Description: handle editor tabs: each can contain a distinct assembly file
*)

/// implement Monaco editor file tabs

module Tabs
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open CommonData
open Refs
open Elmish
open Fable.Core.JsInterop


/// return the id of the last editor in the map of editors
let selectLastTabId editors =
    editors
    |> Map.toList
    |> List.rev
    |> List.head
    |> fst

/// Editor value of a  blank tab
let blankTab =
    { DefaultValue = ""
      FileName = None
      FilePath = None
      IEditor = None
      Saved = true }

/// top-level function to delete tab
let deleteTabUpdate (tabId, editors : Map<int, Editor>, settingsTab) =
    let newEditors = Map.remove tabId editors
    let newTabId = 
        match Map.isEmpty newEditors with
        | true -> -1
        | false -> selectLastTabId newEditors
    let newSettingsTab =
        match settingsTab with
        | Some x when x = tabId -> None
        | x -> 
            editors.[tabId].IEditor?dispose () |> ignore
            x
    newTabId, newEditors, newSettingsTab

/// top-level function to select file tab
let selectFileTabUpdate id editors =
    match Map.isEmpty editors with
    | true -> -1
    | _ -> 
        match Map.containsKey id editors with
        | true -> id
        | _ -> selectLastTabId editors

/// top-level function for opening new tab
let newFileUpdate editors =
    let newTabId = uniqueTabId editors
    let newEditors = Map.add newTabId blankTab editors
    newTabId, newEditors

let attemptToDeleteTabUpdate (tabId, (editors: Map<int, Editor>), dialogBox)
                             id =
    match id with
    | -1 -> dialogBox, Cmd.none
    | _ ->
        match id = tabId, editors.[id].Saved, dialogBox with
        | true, true, _ -> dialogBox, Cmd.ofMsg DeleteTab
        | true, false, None -> Some UnsavedFileDl, Cmd.none
        | _ -> dialogBox, Cmd.none