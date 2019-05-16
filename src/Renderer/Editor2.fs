module Editors2

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Monaco
open Refs

type Props =
    | Width of obj
    | Height of obj
    //| Value of obj
    | DefaultValue of obj
    //| Language of string
    | Theme of string
    | Options of Monaco.Editor.IEditorConstructionOptions
    //| OnChange of (obj * obj -> unit)
    | EditorWillMount of (Monaco.IExports -> unit)
    //| EditorDidMount of (Monaco.Editor.IEditor * Monaco.IExports -> unit)
    | RequireConfig of obj
    | OnChange of (string -> unit)
    | Value of string
    | Language of string
    | IsReadOnly of bool
    | EditorDidMount of (unit -> unit)


let inline editor (props: Props list) : React.ReactElement =
    ofImport "default" "react-monaco-editor" (keyValueList CaseRules.LowerFirst props) []

//type EditorProps =
//    | OnChange of (string -> unit)
//    | Value of string
//    | Language of string
//    | IsReadOnly of bool
//    | EditorDidMount of (unit -> unit)

//let inline editor (props: EditorProps list) : React.ReactElement =
    //ofImport "default" "../../app/js/editor.js" (keyValueList CaseRules.LowerFirst props) []

/// top-level function to update editor when text inside changed
let editorTextChangeUpdate (currentFileTabId : int, editors : Map<int, Editor>)
                           (str : string) =
    let newEditor = 
        { editors.[currentFileTabId] with EditorText = str
                                          Saved = false }
    Map.add currentFileTabId newEditor editors 