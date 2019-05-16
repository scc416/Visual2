(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Settings
    Description: Code to handle persistent settings stored on PC as a file under user data
*)

/// implement electron-style persistent settings via electron-settings module

module Settings2
open Fable.Import.Browser
open Fable.Core.JsInterop
open Fable.Import
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
open Tabs2


let editorFontSize = "editor-font-size"
let simulatorMaxSteps = "simulator-max-steps"
let editorTheme = "editor-theme"
let editorWordWrap = "editor-word-wrap"
let editorRenderWhitespace = "editor-render-whitespace"

let getIntSetting mini maxi (defi : string) setting =
    let (|INT|_|) (n : string) =
        try
            Some(int64 n)
        with
            | e -> Some(defi |> int64)
    match setting with
    | INT n when n >= mini && n <= maxi -> n |> string
    | INT n when n < mini -> mini |> string
    | INT n when n > maxi -> maxi |> string
    | _ -> defi

let getFormSettings (settings : VSettings) =
    let getS (name : string) =
        let input = document.getElementById (name) :?> HTMLInputElement
        input.value
    let getT (name : string) =
        let input = document.getElementById (name) :?> HTMLInputElement
        input.``checked``
    let vs = {
            SimulatorMaxSteps = getIntSetting 0L 1000000000L settings.SimulatorMaxSteps (getS simulatorMaxSteps)
            EditorFontSize = getIntSetting 6L 100L settings.EditorFontSize (getS editorFontSize)
            EditorTheme = getS editorTheme
            EditorWordWrap = getT editorWordWrap
            EditorRenderWhitespace = getT editorRenderWhitespace
            CurrentFilePath = settings.CurrentFilePath
            RegisteredKey = settings.RegisteredKey
            OnlineFetchText = settings.OnlineFetchText
        }
    let newSettings = checkSettings vs
    printfn "Saving settings: %A" newSettings
    setJSONSettings newSettings
    newSettings

//let initFormSettings settings =
    //let setS (name : string) (v : string) =
    //    let input = find
    //    //reft.getElementById (name) :?> HTMLInputElement
    //    //printfn "name=%A,dom=%A, value=%A" name input v
    //    input.value <- v
    //let vs = settings
    //setS simulatorMaxSteps <| (uint64 vs.SimulatorMaxSteps).ToString()
    //setS editorFontSize <| (uint64 vs.EditorFontSize).ToString()
    //setS editorTheme vs.EditorTheme
    //setS editorWordWrap vs.EditorWordWrap
    //setS editorRenderWhitespace vs.EditorRenderWhitespace

// React description for the settings menu
let settingsMenu dispatch (settings : VSettings): ReactElement list =
    let onChange =
        OnChange (fun x -> EditorTextChange "" |> dispatch)
    [ form [ Class "settings-menu editor" ]
           [ div [ Class "float-left" ]
                 [ h4 []
                      [ str "Editor" ]
                   div [ Class "form-group" ]
                       [ label [ Class "settings-label" ]
                               [ str "Font Size" ]
                         br [ ]
                         input [ Type "number"
                                 Id "editor-font-size"
                                 Class "settings-input"
                                 DefaultValue settings.EditorFontSize
                                 Min 6
                                 Max 60
                                 Step 2 
                                 onChange ]]
                   div [ Class "form-group" ]
                       [ label [ Class "settings-label" ]
                               [ str "Theme" ]
                         br [ ]
                         select [ Class "form-control settings-select"
                                  Id "editor-theme" 
                                  DefaultValue settings.EditorTheme 
                                  onChange ]
                                [ option [ Value "one-dark-pro" ]
                                         [ str "One Dark Pro" ]
                                  option [ Value "one-light-pro" ]
                                         [ str "One Light Pro" ]
                                  option [ Value "solarised-dark" ]
                                         [ str "Solarised Dark" ]
                                  option [ Value "solarised-light" ]
                                         [ str "Solarised Light" ] ] ]
                   div [ Class "form-group" ]
                       [ label [ Class "settings-label" ]
                               [ str "Word Wrap" ]
                         br []
                         input [ Type "checkbox"
                                 Id "editor-word-wrap"
                                 DefaultChecked settings.EditorWordWrap
                                 onChange ] ]
                   div [ Class "form-group" ]
                       [ label [ Class "settings-label" ]
                               [ str "Render Whitespace Characters" ]
                         br [ ]
                         input [ Type "checkbox"
                                 Id "editor-render-whitespace"
                                 DefaultChecked settings.EditorRenderWhitespace
                                 onChange ] ] ]
             div []
                 [ h4 []
                      [ str "Simulator" ]
                   div [ Class "form-group" ]
                       [ label [ Class "settings-label" ]
                               [ str "Max Steps"
                                 br [ ]
                                 str "(0 for no max)" ]
                         br [ ]
                         input [ Type "number"
                                 Id "simulator-max-steps"
                                 Class "settings-input"
                                 Min 0
                                 Max 10000000
                                 Step 100 
                                 DefaultValue settings.SimulatorMaxSteps
                                 onChange ] ] ]
             div [ Class "after" ]
                 []
             div [ DOMAttr.OnClick (fun _ -> SaveSetting |> dispatch)]
                 [ div [ Class "btn btn-default" ]
                       [ str "Save and Close Settings" ] ] ] ]

let createSettingsTab editors=
    let id = uniqueTabId editors
    let newSettingTab = 
        { EditorText = ""
          FileName = Some "            Settings"
          FilePath = None
          Saved = true }
    let newEditors =
        Map.add id newSettingTab editors
    newEditors, id