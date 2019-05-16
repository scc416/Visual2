module Tabs2
open EEExtensions
open Refs

/// look in the Editors and find the next unique id
let uniqueTabId (editor : Map<int, Editor>) =
    match Map.isEmpty editor with
    | true -> 
        0
    | false -> 
        let lastid, _ = 
            editor
            |> Map.toList
            |> List.rev
            |> List.head
        lastid + 1

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

/// top-level function to delete tab
let deleteTabUpdate (tabId, editors, settingsTab)
                    id =
    match id = tabId with
    | true -> // only remove the tab if it is the current tab
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
