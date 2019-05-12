module Views2

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
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


let currentRepClassName (rep : Representations) =
    function
    | x when x = rep -> ClassName "btn btn-rep btn-rep-enabled"
    | _ -> ClassName "btn btn-rep"

let currentViewClassName (view : Views)=
    function
    | x when x = view -> ClassName "tab-item active" 
    | _ -> ClassName "tab-item"

let currentTabClassName (view : Views) = 
    function
    | x when x = view  -> ClassName "list-group"
    | _ -> ClassName "list-group invisible"

let byteViewButtonString =
    function
    | false -> str "Enable Byte View"
    | true -> str "Disable Byte View"

let reverseDirectionButtonString =
    function
    | false -> str "Enable Reverse Direction"
    | true -> str "Disable Reverse Direction"

let viewButtonClassName =
    function
    | false -> ClassName "btn full-width btn-byte"
    | true -> ClassName "btn full-width btn-byte btn-byte-active"

let dashboardWidth rep view =
    let w =
        match rep, view with
        | Bin, _ -> "--dashboard-width-binrep"
        | _, Registers -> "--dashboard-width-init-registers"
        | _ -> "--dashboard-width-init-other"
        |> getCustomCSS
    w |> setDashboardWidth

let editorClassName id currentId = ClassName "editor"
    //match id with
    //| x when x = currentId -> ClassName "editor"
    //| _ -> ClassName "editor invisible"

let editor (map : Map<int, Editor>) currentId : (ReactElement list) =
    let tab id =
        div [ 
                editorClassName id currentId 
                id |> fileViewIdFormatter |> Id
            ] []
    div [ ClassName "editor" ] [ str "testing testing"] :: (map
    |> Map.map (fun key _ -> key |> tab)
    |> Map.toList
    |> List.map (fun (_, el) -> el))

let tabGroupClassName id currentId =
    match id with
    | x when x = currentId -> ClassName "tab-file-name" 
    | _ -> ClassName "tab-file-name"

let tabViewClassName id currentId =
    match id with
    | x when x = currentId -> ClassName "tab-item tab-file active" 
    | _ -> ClassName "tab-item tab-file"   