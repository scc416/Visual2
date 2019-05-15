module Files2

open EEExtensions
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Refs

let fileName path =
    path
    |> String.toList
    |> List.rev
    |> List.takeWhile (fun x -> x <> '/')
    |> List.rev
    |> List.toString

let filePathSetting path =
    path
    |> String.toList
    |> List.rev
    |> List.skipWhile (fun x -> x <> '/')
    |> List.rev
    |> List.toString

let openListOfFiles (fLst : string list) : Editor List =
    let readFile (path:string) =
        Node.Exports.fs.readFileSync (path, "utf8")
    fLst
    |> List.map (fun x -> 
        { 
            EditorText = readFile x
            FilePath = Some x
            FileName = x |> fileName |> Some
            Saved = true
        })

let openFile currentFilePath (dispatch : Msg -> Unit) =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile"; "multiSelections" ]) |> Some
    options.filters <- Files.fileFilterOpts
    options.defaultPath <- Some currentFilePath
    let seq = electron.remote.dialog.showOpenDialog (options)
    match isUndefined seq with
    | true -> 
        OpenFile [] |> dispatch
    | false ->
        let fileLst =
            seq
            |> Seq.toList
            |> openListOfFiles
        OpenFile fileLst |> dispatch

let fileFilterOpts =
    ResizeArray [
        createObj [
            "name" ==> "Assembly Code"
            "extensions" ==> ResizeArray [ "s" ]
        ]
    ] |> Some

let saveFileAs filePathSetting (editor : Editor) dispatch : (unit) =
    let options = createEmpty<SaveDialogOptions>
    options.filters <- fileFilterOpts
    let savedFilePath = 
        match editor.FilePath with
        | Some x -> x
        | _ -> filePathSetting
    options.defaultPath <- Some savedFilePath
    let result = electron.remote.dialog.showSaveDialog (options)
    match isUndefined result with
    | true -> 
        SaveAsFile Option.None |> dispatch
    | false -> 
        writeToFile editor.EditorText result
        let fileInfo = result, fileName result
        fileInfo |> Some |> SaveAsFile |> dispatch

let dialogBox m dispatch =
    match m.DialogBox with
    | Some x when x = OpenFileDl ->
        openFile m.Settings.CurrentFilePath 
                 dispatch
    | Some x when x = SaveAsDl ->
        saveFileAs m.Settings.CurrentFilePath 
                   m.Editors.[m.CurrentFileTabId] 
                   dispatch
    | _ -> 
        ()        