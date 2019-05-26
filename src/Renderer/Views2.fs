module Views2

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Tooltips2
open Editors2
open Settings2
open Files2
open ExecutionTop

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
                            DOMAttr.OnClick (fun _ -> rep |> ChangeRep |> dispatch) ]
                          [ rep |> string |> str ] ]

    div [ ClassName "btn-group pull-right" ] 
        [ repButton Hex
          repButton Bin
          repButton Dec
          repButton UDec ]



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

let maxSymbolWidth = 30

let nameSquash maxW name =
    let nameLen = String.length name
    if nameLen <= maxW then name
    else
        let fp = (float maxW) * 0.65 |> int
        let lp = maxW - (fp + 3)
        name.[0..fp - 1] + "..." + name.[nameLen - lp..nameLen - 1]

let symbolView (symbolMap : Map<string,(uint32 * SymbolType)>)=
    let makeRow ((sym : string), (value, typ) : uint32 * ExecutionTop.SymbolType) =
        tr []
           [ td [ ClassName "selectable-text" ] 
                [ str sym ]
             td [ ClassName "selectable-text" ] 
                [ formatter currentRep value |> str ] ]

    let makeGroupHdr (typ : SymbolType) =
        let symName =
            match typ with
            | DataSymbol -> "Data Symbol"
            | CodeSymbol -> "Code Symbol"
            | CalculatedSymbol -> "EQU Symbol"

        tr [] 
           [ th [ ClassName "th-mem" ] 
                [ str symName ]
             th [ ClassName "th-mem" ] 
                [ str "Value" ] ]

    let symTabRows =
        let makeGroupRows (grpTyp, grpSyms) =
            grpSyms
            |> Array.map (fun (sym, addr) -> sym, addr)
            |> Array.sortBy snd
            |> Array.map (fun (sym, addr) -> nameSquash maxSymbolWidth sym, addr)
            |> Array.map makeRow
            |> Array.append [| makeGroupHdr grpTyp |]

        let groupOrder = function
            | (CodeSymbol, _) -> 1
            | (DataSymbol, _) -> 2
            | (CalculatedSymbol, _) -> 3

        symbolMap
        |> Map.toArray
        |> Array.groupBy (fun (_sym, (_addr, typ)) -> typ)
        |> Array.sortBy groupOrder
        |> Array.collect makeGroupRows
        |> Array.toList

    div [ ClassName "list-group" ]
        [ table [ ClassName "table-striped" ]
                symTabRows ]

let memTable memoryMap = 
        match Map.isEmpty memoryMap with
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
                memoryMap
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

let memView memoryMap dispatch = 
   ul [ ClassName "list-group" ]
      [ li [ Class "list-group-item" ]
           [ div [ Class "btn-group full-width" ]
                 [ button [ viewButtonClass byteView
                            DOMAttr.OnClick (fun _ -> ToggleByteView |> dispatch)]
                          [ byteViewButtonString byteView ]
                   button [ viewButtonClass reverseDirection 
                            DOMAttr.OnClick (fun _ -> ToggleReverseView |> dispatch)]
                          [ reverseDirectionButtonString reverseDirection ] ] ]
        li [ Class "list-group" ] (memTable memoryMap)
        li [ ]
           [ div [ Style [ TextAlign "center" ] ]
                 [ b [] [ str "Uninitialized memory is zeroed" ] ] ] ]


let regView (regMap : Map<CommonData.RName, uint32>) = 
    let registerLi rName  = 
        li [ ClassName "list-group-item" ] 
           [ div [ ClassName "btn-group full-width" ] 
                 [ tooltips ((rName |> regTooltipStr |> Content) :: Placement "bottom" :: basicTooltipsPropsLst)
                            [ button [ ClassName "btn btn-reg" ] 
                                     [  rName |> string |> str ] ]
                   span [ ClassName "btn btn-reg-con selectable-text" ] 
                        [ regMap.[rName] |> formatter currentRep |> str ] ] ]
    let registerSet regMap =
        [0 .. 15]
        |> List.map (fun x -> 
            x |> CommonData.register |> registerLi )

    ul [ ClassName "list-group" ] (registerSet regMap)


/// return the view panel on the right
let viewPanel (currentRep, currentView, regMap : Map<CommonData.RName, uint32>)
              (memoryMap : Map<uint32, uint32>, symbolMap, byteView, reverseDirection)
              dispatch = 


    let symbolView = symbolView symbolMap
    let view =
        match currentView with
        | Registers -> regView regMap
        | Memory -> memView memoryMap dispatch
        | Symbols -> symbolView
    div [ ClassName "viewer" ; viewerStyle currentRep ] 
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

