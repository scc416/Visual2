module Tabs2
open EEExtensions
open Refs
open MenuBar2

/// return the id of the last editor in the map of editors
let selectLastTabId editors =
    editors
    |> Map.toList
    |> List.rev
    |> List.head
    |> fst

/// Editor value of a  blank tab
let blankTab =
    {
        EditorText = ""
        FileName = None
        FilePath = None
        Saved = true
    }

let attemptoDeleteTabUpdate (tabId, editors : Map<int, Editor>, settingsTab)
                    id =
    match id = tabId with
    | true -> 
        let callback (result : bool) =
            match result with
            | false -> ()
            | true -> close()
        match editors.[id].Saved with// only remove the tab if it is the current tab
        | true -> showQuitMessage callback
        | false -> ()
        let newEditors = Map.remove id editors
        let newTabId = 
            match Map.isEmpty newEditors with
            | true -> -1
            | false -> selectLastTabId newEditors
        let newSettingsTab =
            match settingsTab with
            | Some x when x = id -> None
            | x -> x
        newTabId, newEditors, newSettingsTab
    | false -> 
        id, editors, settingsTab


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
        | x -> x
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
