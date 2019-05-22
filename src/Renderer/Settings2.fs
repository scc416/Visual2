module Settings2
open Fable.Import.Browser
open Refs
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Tabs2
open MenuBar2

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
    let newSettings = checkSettings vs
    setJSONSettings newSettings
    newSettings

/// React description for the settings menu
let settingsMenu dispatch (settings : VSettings) saved: ReactElement list =
    let onChange =
        OnChange (fun x -> 
            match saved with
            | true -> EditorTextChange |> dispatch
            | false -> ())
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
             div [ DOMAttr.OnClick (fun _ -> SaveSettings |> dispatch)]
                 [ div [ Class "btn btn-default" ]
                       [ str "Save and Close Settings" ] ] ] ]

/// create new setting tab and return new editors map & tab id
let createSettingsTab editors =
    let id = uniqueTabId editors
    let newSettingTab = 
        { DefaultValue = ""
          FileName = Some "            Settings"
          FilePath = None
          Saved = true
          IEditor = None }
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
let saveSettingsUpdate (settings, editors, settingsTab) = 
    let newSettings =
        getFormSettings settings
    // close and remove the setting tab after settings are saved
    let newEditors = Map.remove settingsTab editors
    let newId = selectLastTabId newEditors
    newSettings, newEditors, newId