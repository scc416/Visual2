module Editors2

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Monaco

    module Monaco =

        open Fable.Core.JsInterop

        type Props =
            | Width of obj
            | Height of obj
            | Value of obj
            | DefaultValue of obj
            | Language of string
            | Theme of string
            | Options of Monaco.Editor.IEditorConstructionOptions
            | OnChange of (obj * obj -> unit)
            | EditorWillMount of (Monaco.IExports -> unit)
            | EditorDidMount of (Monaco.Editor.IEditor * Monaco.IExports -> unit)
            | RequireConfig of obj

        let inline editor (props: Props list) : React.ReactElement =
            ofImport "default" "react-monaco-editor" (keyValueList CaseRules.LowerFirst props) []

    module Editor =

        open Fable.Core.JsInterop

        type Props =
            | OnChange of (string -> unit)
            | Value of string
            | Language of string
            | IsReadOnly of bool
            | EditorDidMount of (unit -> unit)

        let inline editor (props: Props list) : React.ReactElement =
            ofImport "default" "./js/Editor.js" (keyValueList CaseRules.LowerFirst props) []