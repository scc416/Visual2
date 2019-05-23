module Editors2

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Monaco
open Monaco.Monaco
open Monaco.Editor
open Refs
open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Settings2

type Props =
    | Width of obj
    | Height of obj
    | DefaultValue of obj
    | Language of string
    | Theme of string
    | Options of obj
    | EditorWillMount of (Monaco.IExports -> unit)
    | EditorDidMount of (Monaco.Editor.IEditor -> unit)
    | RequireConfig of obj
    | OnChange of (string -> unit)
    | Value of string
    | IsReadOnly of bool

let editorOptions vs =
    createObj [

        // User defined settings
        "theme" ==> vs.EditorTheme
        "renderWhitespace" ==> vs.EditorRenderWhitespace
        "fontSize" ==> vs.EditorFontSize
        "wordWrap" ==> vs.EditorWordWrap
        "renderIndentGuides" ==> false
        "fontFamily" ==> "fira-code"
        "fontWeight" ==> "bold"
        "language" ==> "arm";
        "roundedSelection" ==> false;
        "scrollBeyondLastLine" ==> false;
        //"readOnly" ==> readOnly;
        "automaticLayout" ==> true;
        "minimap" ==> createObj [ "enabled" ==> false ];
        "glyphMargin" ==> true
        //"model" ==> model
    ]

let inline editor (props: Props list) : React.ReactElement =
    ofImport "default" "react-monaco-editor" (keyValueList CaseRules.LowerFirst props) []
 
// ***********************************************************************************
//                     Functions Relating to Editor Panel
// ***********************************************************************************

/// decide whether this file name is gonna be bold or not (in the tab header)
/// bold if it is unsaved
let tabHeaderTextStyle =
    function
    | true  -> Style []
    | false -> Style [ FontWeight "bold" ]

/// decide whether this file name is gonna be ending with "*" (in the tab header)
/// ending with "*" if it is unsaved
let fileNameFormat (fileName : string) 
                   (saved : bool) : string =
    match saved with
    | true -> fileName
    | false -> fileName + " *"

let tabNameClass id =
    function
    | Some x when x = id -> ClassName "tab-file-name icon icon-cog" 
    | _ -> ClassName "tab-file-name" 

/// return all the react elements in the editor panel (including the react monaco editor)
let editorPanel (currentFileTabId, editors : Map<int, Editor>, settingsTabId, settings) 
                dispatch =

    /// return the class of the tab header
    /// "active" if it is the current tab
    let tabHeaderClass (id : int) : HTMLAttr =
        match id with
        | x when x = currentFileTabId -> ClassName "tab-item tab-file active" 
        | _ -> ClassName "tab-item tab-file"   

    let editorClass (id : int) : HTMLAttr =
        match id with
        | x when x = currentFileTabId -> ClassName "editor" 
        | _ -> ClassName "editor invisible"

    let editor txt id = 
        let onChange = 
            OnChange (fun _ -> 
                match editors.[id].Saved with
                | true -> EditorTextChange |> dispatch
                | false -> ())
        editor [ onChange
                 settings |> editorOptions |> Options 
                 DefaultValue txt 
                 EditorDidMount (fun x -> UpdateIEditor (x, id) |> dispatch) ]

    let editorDiv id =
        div [ editorClass id
              id |> tabNameIdFormatter |> Key ] 
            [ editor editors.[id].DefaultValue id ]

    /// return a tab header
    let tabHeaderDiv (id : int) (editor : Editor) : ReactElement =

        /// return "untitled.s" if the tab has no filename
        let fileName =
            match editor.FileName with
            | Some x -> x
            | _ -> "Untitled.s"

        div [ tabHeaderClass id
              DOMAttr.OnClick (fun _ -> SelectFileTab id |> dispatch)] 
            [ span [ ClassName "invisible" ] []
              span [ ClassName "icon icon-cancel icon-close-tab" 
                     DOMAttr.OnClick (fun _ -> AttemptToDeleteTab id |> dispatch)] []
              span [ tabNameClass id settingsTabId
                     tabHeaderTextStyle editor.Saved ] 
                   [ editor.Saved |> fileNameFormat fileName |> str ]]
    /// button (actually is a clickable div) that add new tab
    let addNewTabDiv =
        [ div [ ClassName "tab-item tab-item-fixed" 
                DOMAttr.OnClick (fun _ -> NewFile |> dispatch) ]
              [ span [ ClassName "icon icon-plus"] []]]

    /// all tab headers plus the add new tab button
    let tabHeaders =
        let filesHeader = 
            editors
            |> Map.map (fun id editor -> tabHeaderDiv id editor)
            |> Map.toList
            |> List.map (fun (_, name) -> name)

        addNewTabDiv 
        |> List.append filesHeader
        |> div [ ClassName "tab-group tabs-files" ]

    let overlay =
        div [ ClassName "invisible darken-overlay" ] []
    /// the editor
    let editorViewDiv =
        let editorsLst = 
            editors
            |> Map.toList
            |> List.map (fun (key, _) -> editorDiv key)
        match settingsTabId with
        | Some x when x = currentFileTabId -> 
            let settingEl = settingsMenu dispatch settings editors.[currentFileTabId].Saved
            settingEl :: editorsLst
        | _ -> 
            editorsLst

    tabHeaders :: editorViewDiv