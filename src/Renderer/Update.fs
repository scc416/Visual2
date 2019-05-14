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

let openFileModel model =
    let newId = uniqueTabId model.Editors
    let newEditor =
        model.Settings.CurrentFilePath
        |> MenuBar2.openFile
    let length = List.length newEditor
    match length with
    | 0 -> model
    | _ ->
        let newEditors =
            newEditor
            |> List.filter (fun x ->
                let empty = Map.isEmpty model.Editors
                let exist = Map.forall (fun _ value -> 
                    value.FilePath <> x.FilePath) model.Editors
                match empty, exist with
                | false, false -> false
                | _ -> true)
            |> List.zip [newId .. newId + length - 1]
            |> Map.ofList
        let mapMerge newMap = Map.fold (fun map key value -> Map.add key value map) newMap
        let mergedEditors = mapMerge newEditors model.Editors
        let openedEditor = newEditor |> List.head
        let newId = 
            mergedEditors
            |> Map.tryFindKey (fun _ value -> value.FilePath = openedEditor.FilePath)
        { 
            model with Editors = mergedEditors
                       CurrentFileTabId = newId.Value 
                       Settings = { model.Settings with 
                                        CurrentFilePath = 
                                            mergedEditors.[newId.Value].FilePath.Value }

                     }