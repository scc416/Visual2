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
let deleteTabUpdate (info, settingsTab) =
    let newEditors = Map.remove info.TabId info.Editors
    let newTabId = 
        match Map.isEmpty newEditors with
        | true -> -1
        | false -> selectLastTabId newEditors
    let newSettingsTab =
        match settingsTab with
        | Some x when x = info.TabId -> None
        | x -> 
            match x = Some info.TabId with
            | true ->
                info.Editors.[info.TabId].IEditor?dispose () |> ignore
            | _ -> ()
            x
    { TabId = newTabId
      Editors =  newEditors }, newSettingsTab

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
    { TabId = newTabId
      Editors = newEditors }

let attemptToDeleteTabUpdate (info, dialogBox)
                             id =
    match id with
    | -1 -> Cmd.none
    | _ ->
        match id = info.TabId, info.Editors.[id].Saved with
        | true, true -> Cmd.ofMsg DeleteTab
        | true, false -> UnsavedFileDl |> UpdateDialogBox |> Cmd.ofMsg
        | _ -> Cmd.none