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
    
                   

let alertDialog txt dispatch = 
    showVexAlert txt (fun _ -> CloseDialog |> dispatch )

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
    | Some OpenFileDl ->
        openFile currentFilePath 
                 dispatch
    | Some SaveAsDl ->
        saveFileAs currentFilePath 
                   editors.[tabId]
                   dispatch
    | Some UnsavedFileDl ->
        closeTabDialog (editors.[tabId].FileName, settingTab, tabId)
                       dispatch
    | Some QuitDl ->
        ExitIfOK dispatch
    | Some (Alert txt) ->
        alertDialog txt dispatch
    | Some StepBackDl ->
        showVexValidatedPrompt 
            "steps back" 
            validPosInt 
            (fun x -> 
                CloseDialog |> dispatch
                x |> int64 |> StepCodeBackBy |> dispatch) 
            "Number of steps back"
            (CloseDialog |> dispatch)
    | Some StepDl ->
        showVexValidatedPrompt 
            "steps forward" 
            validPosInt 
            (fun x -> 
                CloseDialog |> dispatch
                (ExecutionTop.NoBreak, (int64 x)) |> RunEditorTab |> dispatch) 
            "Number of steps forward" 
            (CloseDialog |> dispatch)
    | Option.None -> 
        ()

let attemptToExitUpdate editors (dialogBox: DialogBox option) =
    match dialogBox with
    | Option.None -> 
        let allSaved = 
            editors 
            |> Map.forall (fun _ value -> value.Saved = true) 
        match allSaved with
        | true -> dialogBox, Cmd.ofMsg Exit
        | _ -> 
            dialogBox 
            |> dialogBoxUpdate (Some QuitDl), Cmd.none
    | _ -> 
        dialogBox, Cmd.none