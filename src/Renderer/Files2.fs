﻿module Files2

open EEExtensions
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Refs
open Tabs2
open MenuBar2

/// merge 2 maps into 1
/// if key repeated, the ones in the old map are kept
let mapMerge newMap = 
    newMap 
    |> Map.fold (fun map key value -> 
        Map.add key value map)

/// return the file name from the file path
let fileName path =
    path
    |> String.toList
    |> List.rev
    |> List.takeWhile (fun x -> x <> '/')
    |> List.rev
    |> List.toString

/// return file path without file name
let filePathSetting path =
    path
    |> String.toList
    |> List.rev
    |> List.skipWhile (fun x -> x <> '/')
    |> List.rev
    |> List.toString

/// format the list of files into a list of Editors
let openLstOfFiles (fLst : string list) : Editor List =
    let readFile (path:string) =
        Node.Exports.fs.readFileSync (path, "utf8")
    fLst
    |> List.map (fun x -> 
        { EditorText = readFile x
          FilePath = Some x
          FileName = x |> fileName |> Some
          Saved = true })

/// open file dialog
let openFile currentFilePath (dispatch : Msg -> Unit) =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile"; "multiSelections" ]) |> Some
    options.filters <- Files.fileFilterOpts
    options.defaultPath <- Some currentFilePath
    // path of the opened files
    let seq = electron.remote.dialog.showOpenDialog (options) /// open dialog
    let fileLst =
        match isUndefined seq with
        | true -> /// the dialog is cancelled, so seq is undefined
            []
        | false ->
            seq
            |> Seq.toList
            |> openLstOfFiles
    OpenFile fileLst |> dispatch

let fileFilterOpts =
    ResizeArray [
        createObj [
            "name" ==> "Assembly Code"
            "extensions" ==> ResizeArray [ "s" ]
        ]
    ] |> Some

/// save file dialog
let saveFileAs filePathSetting (editor : Editor) dispatch : (unit) =
    let options = createEmpty<SaveDialogOptions>
    options.filters <- fileFilterOpts
    let savedFilePath = 
        match editor.FilePath with
        | Some x -> x
        | _ -> filePathSetting
    options.defaultPath <- Some savedFilePath
    /// path of the saved file
    let result = electron.remote.dialog.showSaveDialog (options) ///open the save file dialog
    match isUndefined result with
    | true -> 
        SaveAsFile Option.None |> dispatch
    | false -> 
        writeToFile editor.EditorText result
        let fileInfo = result, fileName result
        fileInfo |> Some |> SaveAsFile |> dispatch

let closeTabDialog fileName dispatch= 
    let fileName =
        match fileName with
        | Some x -> x
        | _ -> "Untitled.s"
    let dialog =
        Browser.window.confirm (
            sprintf "You have unsaved changes, are you sure you want to close %s?"
                    (fileName))
    match dialog with
    | true -> DeleteTab |> dispatch
    | _ -> ()



/// top-level function for saving file
/// open the save dialog when necessary
let saveFileUpdate (tabId, editors : Map<int, Editor>) =
    let mutable newDialog = Option.None
    let mutable newEditors = editors
    match tabId with
    | -1 -> 
        ()
    | id -> 
        let filePath = editors.[id].FilePath
        match filePath with
        | Option.None -> 
            newDialog <- Some SaveAsDl /// open the dialog
        | Some fPath ->
            let currentEditor = editors.[id]
            writeToFile currentEditor.EditorText fPath
            newEditors <- 
                Map.add id
                        { currentEditor with Saved = true }
                        editors
    newDialog, newEditors

/// top-level function for save file as
/// open the save file dialog when necessary
let saveAsFileDialogUpdate (tabId, dialogBox) : DialogBox option =
    match dialogBox, tabId with
    | _, -1 | Some _, _ -> dialogBox /// make sure sure no other dialog is opened and there is at least one tab
    | _ -> Some SaveAsDl

/// top-level function for opening up the open file dialog
let openFileDialogUpdate =
    function
    | Option.None -> Some OpenFileDl /// make sure no other dialog is opened
    | x -> x

/// top-level function for opening up the open file dialog
let saveAsFileUpdate (editors : Map<int, Editor>, tabId, filePathSettingStr)
                     fileInfo = 
    match fileInfo with
    | Option.None -> 
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

/// top-level function for opening file
let openFileUpdate (oldEditors : Map<int, Editor>, filePath, id) 
                   editor =
    let newId = uniqueTabId oldEditors
    let length = List.length editor
    match length with
    | 0 -> // no file is selected to be opened
        oldEditors, filePath, id
    | _ ->
        let newEditors =
            editor
            // zip it with number so it can be convert into map
            // number start at the unique tab id to avoid replacement
            |> List.zip [newId .. newId + length - 1]
            |> List.filter (fun (_, x) ->
                // check if the files are already opened
                oldEditors
                |> Map.forall (fun _ value -> 
                    value.FilePath <> x.FilePath))
            |> Map.ofList
        let mergedEditors = mapMerge newEditors oldEditors
        let currentEditor = List.head editor // find the first opened file
        let newId = // make the first file as current tab
            mergedEditors
            |> Map.findKey (fun _ value -> 
                value.FilePath = currentEditor.FilePath)
        mergedEditors,
        filePathSetting mergedEditors.[newId].FilePath.Value, 
        newId