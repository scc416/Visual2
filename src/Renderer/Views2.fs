module Views2

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Tooltips2
open Editors2
open Settings2

let dashboardStyle rep =
    let width = 
        match rep with
        | Bin  -> 500
        | _ -> 350
    Style [ MaxWidth width ; MinWidth width ]

let viewerStyle rep =
    let width = 
        match rep with
        | Bin  -> 500
        | _ -> 350
    Style [ MaxWidth width
            MinWidth width
            Left ("100vw - " + string width) ]

// ***********************************************************************************
//                   Functions Relating to Representation Buttons
// ***********************************************************************************

/// return the set of representation buttons
let repButtons (currentRep : Representations) 
               (dispatch : Msg -> unit) : ReactElement =

    /// return a representation button
    let repButton (rep : Representations) =

        /// return the class of a representation button
        let currentRepButtonsClass =
            function
            | x when x = currentRep -> ClassName "btn btn-rep btn-rep-enabled"
            | _ -> ClassName "btn btn-rep"
        tooltips ((rep |> repTooltipStr |> Content) :: 
                  Placement "bottom" :: 
                  defaultTooltipsPropsLst)
                 [ button [ currentRepButtonsClass rep
                            DOMAttr.OnClick (fun _ -> rep |> ChangeRep |> dispatch)]
                          [ rep |> string |> str ] ]

    div [ ClassName "btn-group pull-right" ] 
        [ repButton Hex
          repButton Bin
          repButton Dec
          repButton UDec ]
         
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
                     DOMAttr.OnClick (fun _ -> DeleteTab id |> dispatch)] []
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
        match currentFileTabId, settingsTabId with
        | -1, _ ->
            []
        | _, Some x when x = currentFileTabId -> 
            settingsMenu dispatch settings
        | _ -> 
            let model =
                [ editor [ //Value editors.[currentFileTabId].EditorText
                           OnChange (EditorTextChange >> dispatch)
                           settings |> editorOptions |> Options  
                           EditorDidMount (fun _  -> NewFile |> dispatch )]]
            //let getmodel = getmodel model

            model
    tabHeaders :: editorViewDiv

// ***********************************************************************************
//                   Functions Relating to View Panel (on the right)
// ***********************************************************************************

/// return the set of view buttons for the panel on the right
let viewButtons currentView dispatch =
    /// return class of the view button
    /// active if it is the current view
    let currentViewClass =
        function
        | x when x = currentView -> ClassName "tab-item active" 
        | _ -> ClassName "tab-item"

    /// return a view button
    let viewButton view =
        tooltips ((view |> viewTooltipStr |> Content) :: Placement "bottom" :: defaultTooltipsPropsLst)
                 [ div [ currentViewClass view
                         DOMAttr.OnClick (fun _ -> view |> ChangeView |> dispatch) ] 
                       [ view |> string |> str ]]
    div []
        [ ul [ ClassName "list-group" ] 
             [ li [ ClassName "list-group-item"
                    Style [ Padding 0 ]] 
                  [ div [ ClassName "tab-group full-width" ]
                        [ viewButton Registers
                          viewButton Memory
                          viewButton Symbols ]]]]

/// return the string in byte view button (memory view)
let byteViewButtonString =
    function
    | false -> str "Enable Byte View"
    | true -> str "Disable Byte View"

/// return the string in byte view button (memory view)
let reverseDirectionButtonString =
    function
    | false -> str "Enable Reverse Direction"
    | true -> str "Disable Reverse Direction"

/// return the string in view button (memory view)
let viewButtonClass =
    function
    | false -> ClassName "btn full-width btn-byte"
    | true -> ClassName "btn full-width btn-byte btn-byte-active"

/// return the view panel on the right
let viewPanel model dispatch = 
    let registerLi rName = 
        li [ ClassName "list-group-item" ] 
           [ div [ ClassName "btn-group full-width" ] 
                 [ tooltips ((rName |> regTooltipStr |> Content) :: Placement "bottom" :: basicTooltipsPropsLst)
                            [ button [ ClassName "btn btn-reg" ] 
                                     [  rName |> string |> str ] ]
                   span [ ClassName "btn btn-reg-con selectable-text" ] 
                        [ model.RegMap.[rName] |> formatter model.CurrentRep |> str ] ] ]
    let registerSet =
        [0 .. 15]
        |> List.map (fun x -> 
            x |> CommonData.register |> registerLi )

    let regView = ul [ ClassName "list-group" ] registerSet

    let memTable = 
        match Map.isEmpty model.MemoryMap with
        | true -> 
            []
        | false -> 
            let rows =
                let row address value =
                    tr []
                       [ td [ Class "td-mem" ]
                            [ address |> string |> str ]
                         td [ Class "td-mem" ]
                            [ value |> string |> str ] ] 
                model.MemoryMap
                |> Map.map (fun key value -> row key value)
                |> Map.toList
                |> List.map (fun (_, el) -> el)
            [ div [ Class "list-group-item"
                    Style [ Padding "0px" ] ]
                  [ table [ Class "table-striped" ]
                          (tr [ Class "tr-head-mem" ]
                              [ th [ Class "th-mem" ]
                                   [ str "Address" ]
                                th [ Class "th-mem" ]
                                   [ str "Value" ]] :: rows ) ] ]

    let memView = 
       ul [ ClassName "list-group" ]
          [ li [ Class "list-group-item" ]
               [ div [ Class "btn-group full-width" ]
                     [ button [ viewButtonClass model.ByteView
                                DOMAttr.OnClick (fun _ -> ToggleByteView |> dispatch)]
                              [ byteViewButtonString model.ByteView ]
                       button [ viewButtonClass model.ReverseDirection 
                                DOMAttr.OnClick (fun _ -> ToggleReverseView |> dispatch)]
                              [ reverseDirectionButtonString model.ReverseDirection ] ] ]
            li [ Class "list-group" ] memTable
            li [ ]
               [ div [ Style [ TextAlign "center" ] ]
                     [ b [] [ str "Uninitialized memory is zeroed" ] ] ] ]
    let symbolView =
        div [ ClassName "list-group" ]
            [ table [ ClassName "table-striped" ]
                    []]
    let view =
        match model.CurrentView with
        | Registers -> regView
        | Memory -> memView
        | Symbols -> symbolView
    div [ ClassName "viewer" ; viewerStyle model.CurrentRep ] 
        [ view ]

let footer (flags : CommonData.Flags) =
    let footerDiv (letter) (flag : bool) =
        let text =
            match flag with
            | true -> "1"
            | false -> "0"
        div [ ClassName "btn-group btn-flag-group" 
              Style [ Margin 5 ] ] 
            [ button [ ClassName "btn btn-flag" ] 
                     [ str letter ]
              button [ ClassName "btn btn-flag-con" ] 
                     [ str text ] ]
    footer [ ClassName "toolbar toolbar-footer" ] 
           [ tooltips (Content flagTooltipStr :: Placement "top" :: basicTooltipsPropsLst)
                      [ div [ ClassName "pull-right" ]
                            [ footerDiv "N" flags.C
                              footerDiv "Z" flags.Z
                              footerDiv "C" flags.C
                              footerDiv "V" flags.V ] ] ]