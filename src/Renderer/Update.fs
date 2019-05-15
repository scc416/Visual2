module Update

open Refs
open Elmish
open Elmish.React
open Elmish.HMR
open Elmish.Debug
open Elmish.Browser.Navigation
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Monaco
open Monaco.Monaco.Languages
open Monaco.Monaco
open Views2
open Tabs2
open Editors2
open MenuBar2
open Files2

let mapMerge newMap = 
    newMap 
    |> Map.fold (fun map key value -> 
        Map.add key value map)

let openFileUpdate (oldEditors : Map<int, Editor>, editor : Editor List)
                   filePath 
                   id =
    let newId = uniqueTabId oldEditors
    let length = List.length editor
    match length with
    | 0 -> 
        oldEditors, filePath, id
    | _ ->
        let newEditors =
            let empty = Map.isEmpty oldEditors
            editor
            |> List.zip [newId .. newId + length - 1]
            |> List.filter (fun (_, x) ->
                let exist = 
                    oldEditors
                    |> Map.forall (fun _ value -> 
                        value.FilePath <> x.FilePath)
                match empty, exist with
                | false, false -> false
                | _ -> true)
            |> Map.ofList
        let mergedEditors = mapMerge newEditors oldEditors
        let currentEditor = List.head editor
        let newId = 
            mergedEditors
            |> Map.findKey (fun _ value -> 
                value.FilePath = currentEditor.FilePath)
        mergedEditors,
        filePathSetting mergedEditors.[newId].FilePath.Value, 
        newId
              
let selectFileTabUpdate id editors =
    match Map.isEmpty editors with
    | true -> -1
    | _ -> 
        match Map.containsKey id editors with
        | true -> id
        | _ -> selectLastTabId editors

let editorTextChangeUpdate str currentFileTabId (editors : Map<int, Editor>) =
    let newEditor = 
        { editors.[currentFileTabId] with EditorText = str
                                          Saved = false }
    Map.add currentFileTabId 
            newEditor
            editors        

let deleteTabUpdate id tabId editors =
    match id = tabId with
    | true ->
        let newEditors = Map.remove id editors
        let newTabId = 
            match Map.isEmpty newEditors with
            | true -> -1
            | false -> selectLastTabId newEditors
        newTabId, newEditors
    | false -> 
        id, editors

let saveFileUpdate tabId (editors : Map<int, Editor>) =
    match tabId with
    | -1 -> 
        None, editors
    | id -> 
        let filePath = editors.[id].FilePath
        match filePath with
        | None -> 
            Some SaveAsDl, editors
        | Some fPath ->
            let currentEditor = editors.[id]
            writeToFile currentEditor.EditorText fPath
            let newEditors = 
                Map.add id
                        { currentEditor with Saved = true }
                        editors
            None, newEditors

let saveAsFileDialogUpdate tabId 
                           dialogBox : DialogBox option =
    match dialogBox, tabId with
    | _, -1 | Some _, _ -> dialogBox
    | _ -> Some SaveAsDl

let openFileDialogUpdate =
    function
    | None -> Some OpenFileDl
    | x -> x

let saveAsFileUpdate (editors : Map<int, Editor>) 
                     (tabId, filePathSettingStr)
                     fileInfo = 
    match fileInfo with
    | None -> 
        editors, filePathSettingStr
    | Some (filePath, fileName) ->
        let newEditor =
            { editors.[tabId] with FilePath = Some filePath
                                   FileName = Some fileName
                                   Saved = true }
        let newEditors = 
            editors
            |> Map.add tabId
                       newEditor
            |> Map.filter (fun key value ->
                key = tabId ||
                value.FilePath <> newEditor.FilePath)
        let newFilePathSettings = filePathSetting filePath
        newEditors, newFilePathSettings