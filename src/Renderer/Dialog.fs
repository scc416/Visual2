module Dialog

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Files
open Elmish


let closeTabDialog (fileName : string option, settingTab : int option, currentId)
                   dispatch = 
    let showFileName =
        match fileName with
        | Some x -> 
            match settingTab with
            | Some y when y = currentId -> "Settings"
            | _ -> x
        | _ -> "Untitled.s"
    let dialog =
        Browser.window.confirm (
            sprintf "You have unsaved changes, are you sure you want to close %s?"
                    (showFileName))
    match dialog with
    | true -> DeleteTab |> dispatch
    | _ -> CloseDialog |> dispatch

let aboutDialog dispatch = 
    (showVexAlert2 (sprintf "<h4>VisUAL2 ARM Simulator v%s</h4> " Refs.appVersion +
                       "(c) 2018, Imperial College <br> Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
                       " (F# reimplementation), with special mention to Thomas Carrotti," +
                       " Lorenzo Silvestri, and HLP Team 10")) 
                   (fun _ -> CloseDialog |> dispatch )
                   

let alertDialog txt dispatch = 
    showVexAlert2 txt (fun _ -> CloseDialog |> dispatch )
                   
let aboutDialogUpdate =
    function
    | Some x -> Some x
    | Option.None -> Some AboutDl

let showQuitMessage (callBack : bool -> unit) =
    let mess = "You have unsaved changes. Are you sure you want to exit and lose changes?"
    let buttons = [ "Save"; "Exit without saving" ]
    Refs.showVexConfirm mess callBack
    
let close() = electron.ipcRenderer.send "doClose" |> ignore

/// Display dialog asking for confirmation.
let ExitIfOK dispatch =
    //let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result : bool) =
        match result with
        | false -> CloseDialog |> dispatch
        | true -> Exit |> dispatch
    showQuitMessage callback

/// determine if any dialog box has to be opened
let dialogBox (currentFilePath, editors : Map<int, Editor>, tabId: int, settingTab : int option)
              dispatch =
    function
    | Some x when x = OpenFileDl ->
        openFile currentFilePath 
                 dispatch
    | Some x when x = SaveAsDl ->
        saveFileAs currentFilePath 
                   editors.[tabId]
                   dispatch
    | Some x when x = UnsavedFileDl ->
        closeTabDialog (editors.[tabId].FileName, settingTab, tabId)
                       dispatch
    | Some x when x = AboutDl ->
        aboutDialog dispatch
    | Some x when x = QuitDl ->
        ExitIfOK dispatch
    | Some x when x = NoFileTabDl ->
        alertDialog "No file tab in editor to run!" dispatch
    | Some x when x = ResettingEmulatorDl ->
        alertDialog "Resetting emulator for new execution" dispatch
    | _ -> 
        ()

let attemptToExitUpdate editors (dialogBox: DialogBox option) =
    match dialogBox with
    | Option.None -> 
        let anyUnsaved = 
            editors 
            |> Map.forall (fun _ value -> value.Saved = true) 
        match anyUnsaved with
        | true -> dialogBox, Cmd.ofMsg Exit
        | _ -> Some QuitDl, Cmd.none
    | _ -> 
        dialogBox, Cmd.none