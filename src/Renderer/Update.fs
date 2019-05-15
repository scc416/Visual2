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

let openFileUpdate editors filePath id =
    let newId = uniqueTabId editors
    let newEditor = openFile filePath
    let length = List.length newEditor
    match length with
    | 0 -> 
        editors, filePath, id
    | _ ->
        let newEditors =
            let empty = Map.isEmpty editors
            newEditor
            |> List.filter (fun x ->
                let exist = 
                    editors
                    |> Map.forall (fun _ value -> 
                        value.FilePath <> x.FilePath) 
                match empty, exist with
                | false, false -> false
                | _ -> true)
            |> List.zip [newId .. newId + length - 1]
            |> Map.ofList
        let mapMerge newMap = 
            newMap 
            |> Map.fold (fun map key value -> 
                Map.add key 
                        value 
                        map)
        let mergedEditors = mapMerge newEditors editors
        let openedEditor = List.head newEditor
        let newId = 
            mergedEditors
            |> Map.tryFindKey (fun _ value -> 
                value.FilePath = openedEditor.FilePath)
        mergedEditors, 
        mergedEditors.[newId.Value].FilePath.Value, 
        newId.Value
              
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