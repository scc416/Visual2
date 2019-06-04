module Settings

open Fable.Import.Browser
open Refs
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop
open Fable.Import
open Tabs
open CommonData
open Fable.Core
open Node.Exports

/// A reference to the settings for the app
/// persistent using electron-settings
let settings : obj = electron.remote.require "electron-settings"

let checkPath (p : string) =
    let p' = path.dirname p
    try
        let stat' = fs.statSync (U2.Case1 p')
        let stat = fs.statSync (U2.Case1 p)
        match (stat.isDirectory(), stat'.isDirectory()) with
        | true, _ -> p
        | false, true -> p'
        | _ -> os.homedir()
    with
        | e -> os.homedir()

let setJSONSettings setting =
    let setSetting (name : string) (value : string) =
        printf "Saving JSON: %A" value
        settings?set (name, value) |> ignore
    printfn "Saving settings to this PC: %A" setting
    setSetting "JSON" (Fable.Import.JS.JSON.stringify setting)


let getJSONSettings initSettings =
    let json = settings?get ("JSON", "undefined")
    printfn "Getting settings"
    match json = "undefined" with
    | true ->
            printfn "No JSON settings found on this PC"
            setJSONSettings()
            initSettings
    | false ->
        try
            let vs = (Fable.Import.JS.JSON.parse json) :?> VSettings
            vs
        with
        | e ->
            printfn "Parse failed: using default settings"
            initSettings


let checkSettings (vs : VSettings) vso =
    try
        let checkNum (n : string) (min : int64) (max : int64) (def : string) =
            match int64 n with
            | x when x > max -> def
            | x when x < min -> def
            | x -> x.ToString()
        {
        vs with
            EditorTheme =
                match List.tryFind (fun (th, _) -> (th = vs.EditorTheme)) themes with
                | Some _ -> vs.EditorTheme
                | _ -> printfn "Setting theme to default"
                       vso.EditorTheme
            SimulatorMaxSteps =
                checkNum vs.SimulatorMaxSteps 0L System.Int64.MaxValue vso.SimulatorMaxSteps
            EditorFontSize =
                checkNum vs.EditorFontSize minFontSize maxFontSize vso.EditorFontSize
            CurrentFilePath = checkPath vs.CurrentFilePath
        }
    with
        | _ -> printf "Error parsing stored settings: %A" vs
               vs


/// string for the id, to obtain the value in input
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


/// examine, save (onto user's computer) and return the new settings
let getFormSettings (settings : VSettings) =
    let getS (name : string) =
        let input = document.getElementById (name) :?> HTMLInputElement
        input.value
    /// for checked box
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
    let newSettings = checkSettings vs settings
    setJSONSettings newSettings
    newSettings

/// React description for the settings menu
let settingsMenu dispatch (settings : VSettings) saved: ReactElement =
    let onChange =
        OnChange (fun x -> 
            match saved with
            | true -> EditorTextChange |> dispatch
            | false -> () )
    form [ Class "settings-menu editor" ]
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
                                [ option [ Value "vs" ]
                                         [ str "vs" ]
                                  option [ Value "vs-dark" ]
                                         [ str "vs-dark" ]
                                  option [ Value "hc-black" ]
                                         [ str "hc-black" ] ] ]
                                //[ option [ Value "one-dark-pro" ]
                                  //       [ str "One Dark Pro" ]
                                  //option [ Value "one-light-pro" ]
                                  //       [ str "One Light Pro" ]
                                  //option [ Value "solarised-dark" ]
                                  //       [ str "Solarised Dark" ]
                                  //option [ Value "solarised-light" ]
                                         //[ str "Solarised Light" ] ] ]
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
             div [ DOMAttr.OnClick (fun _ -> SaveSettings |> dispatch)]
                 [ div [ Class "btn btn-default" ]
                       [ str "Save and Close Settings" ] ] ] 

/// create new setting tab and return new editors map & tab id
let createSettingsTab editors =
    let id = uniqueTabId editors
    let newSettingTab = 
        { DefaultValue = ""
          FileName = Some "            Settings"
          FilePath = None
          IEditor = None 
          Saved = true }
    let newEditors =
        Map.add id newSettingTab editors
    newEditors, id

/// top-level function to select the settings tab
/// create a setting tab when necessary
let selectSettingsTabUpdate (settingsTab, editors) =
    match settingsTab with
    | None -> createSettingsTab editors
    // setting tab already exists
    | Some x -> editors, x

/// top-level function to save settings
let saveSettingsUpdate (settings, editors, settingsTab, tabId, iExports) = 
    let newSettings =
        getFormSettings settings
    // close and remove the setting tab after settings are saved
    let newEditors = Map.remove settingsTab editors
    let newId = 
        match Map.isEmpty newEditors with
        | true -> -1
        | _ -> selectLastTabId newEditors
    match newSettings.EditorTheme = settings.EditorTheme with
    | false -> iExports?editor?setTheme (newSettings.EditorTheme)
    | true -> ()
    newSettings, newEditors, newId