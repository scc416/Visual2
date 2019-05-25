module Editors

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Monaco

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

let inline editor (props: Props list) : React.ReactElement =
    ofImport "default" "react-monaco-editor" (keyValueList CaseRules.LowerFirst props) []