module Dialog

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Files2

let closeTabDialog fileName dispatch = 
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

let aboutDialog dispatch = 
    (showVexAlert2 (sprintf "<h4>VisUAL2 ARM Simulator v%s</h4> " Refs.appVersion +
                       "(c) 2018, Imperial College <br> Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
                       " (F# reimplementation), with special mention to Thomas Carrotti," +
                       " Lorenzo Silvestri, and HLP Team 10")) 
                   (fun _ -> CloseAboutDialog |> dispatch )

/// determine if any dialog box has to be opened
let dialogBox (dialogBox, currentFilePath, editors : Map<int, Editor>, tabId: int)
              dispatch =
    match dialogBox with
    | Some x when x = OpenFileDl ->
        openFile currentFilePath 
                 dispatch
    | Some x when x = SaveAsDl ->
        saveFileAs currentFilePath 
                   editors.[tabId]
                   dispatch
    | Some x when x = UnsavedFileDl ->
        closeTabDialog editors.[tabId].FileName 
                       dispatch
    | Some x when x = AboutDl ->
        aboutDialog dispatch
    | _ -> 
        ()

let aboutDialogUpdate =
    function
    | Some x -> Some x
    | Option.None -> Some AboutDl