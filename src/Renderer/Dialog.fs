module Dialog

open Refs
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Files
open Fable.Core
open Microsoft.FSharp.Collections
open EEExtensions
open Elmish
open Result
open FSharp.Core

//-------------------------------------------VEX-JS-----------------------------------------------

/// interfaces to vex.js
[<Emit("require('vex-js');")>]
let vex : obj = jsNative

[<Emit("require('vex-dialog');")>]
let vexDialog : obj = jsNative

vex?defaultOptions?className <- "vex-theme-default"

vex?registerPlugin vexDialog

let showVexConfirm (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?confirm (createObj [
        "unsafeMessage" ==> htmlMessage;
        "callback" ==> callBack ])
    ()

let showVexPrompt (placeHolder : string) (callBack : string -> unit) (htmlMessage : string) =
    vex?dialog?prompt (createObj [
        "unsafeMessage" ==> htmlMessage
        "placeholder" ==> placeHolder
        "callback" ==> callBack
        ])
    ()

let showVexAlert (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?alert (createObj [ "unsafeMessage" ==> htmlMessage 
                                  "callback" ==> callBack ])
    ()

let validPosInt s =
    match s with
    | null -> Error ""
    | x ->
        printfn "Error %s" x
        x
        |> System.Int32.TryParse
        |> function
            | true, n when n > 0 -> Ok n
            | true, n -> Error "number must be greater than 0"
            | false, _ -> Error "Input a positive integer"

let showVexValidatedPrompt (placeHolder : string) 
                           (validator : string -> Result<'T, string>) 
                           (callBack : 'T -> unit) 
                           (htmlMessage : string) 
                           (callBack2 : Unit) =
    let cb (s : string) =
        match (unbox s) with
        | false -> ()
        | _ ->
            validator s
            |> function
                | Ok valid -> 
                    callBack valid
                    callBack2
                | Error _ -> 
                    callBack2
    showVexPrompt placeHolder cb htmlMessage

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
    showVexConfirm mess callBack

let showAlert (message : string) (detail : string) =
    showVexAlert <|
        sprintf """<p style="text-align:center"><b>%s</b><br>%s</p>""" detail message

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