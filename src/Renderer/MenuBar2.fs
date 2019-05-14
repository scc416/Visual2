module MenuBar2

open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Base
open Refs

let openListOfFiles (fLst : string list) : Editor List =
    let readFile (path:string) =
        Node.Exports.fs.readFileSync (path, "utf8")
    let fileName path =
        path
        |> String.toList
        |> List.rev
        |> List.takeWhile (fun x -> x <> '/')
        |> List.rev
        |> List.toString
    fLst
    |> List.map (fun x -> 
        { 
            Saved = true
            EditorText = readFile x
            FileName = x |> fileName |> Some
            Path = Some x
        })

let openFile currentFilePath =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile"; "multiSelections" ]) |> Some
    options.filters <- Files.fileFilterOpts
    options.defaultPath <- Some currentFilePath
    electron.remote.dialog.showOpenDialog (options)
    |> Seq.toList
    |> openListOfFiles