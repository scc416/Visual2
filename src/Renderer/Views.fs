module Views

open Refs
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Tooltips
open Editors
open Settings
open Files
open ExecutionTop

open Fable.Core.JsInterop
open Fable.Import
open Files
open Fable.Core
open Microsoft.FSharp.Collections
open EEExtensions
open Elmish
open FSharp.Core

//-------------------------------------------VEX-JS-----------------------------------------------

/// interfaces to vex.js
[<Emit("require('vex-js');")>]
let vex : obj = jsNative

[<Emit("require('vex-dialog');")>]
let vexDialog : obj = jsNative

vex?defaultOptions?className <- "vex-theme-default"

vex?registerPlugin vexDialog

let showVexConfirm (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?confirm (createObj [
        "unsafeMessage" ==> htmlMessage;
        "callback" ==> callBack ])
    ()

let showVexPrompt (placeHolder : string) (callBack : string -> unit) (htmlMessage : string) =
    vex?dialog?prompt (createObj [
        "unsafeMessage" ==> htmlMessage
        "placeholder" ==> placeHolder
        "callback" ==> callBack
        ])
    ()

let showVexAlert (htmlMessage : string) (callBack : bool -> unit) =
    vex?dialog?alert (createObj [ "unsafeMessage" ==> htmlMessage 
                                  "callback" ==> callBack ])
    ()

let validPosInt s =
    match s with
    | null -> Error ""
    | x ->
        printfn "Error %s" x
        x
        |> System.Int32.TryParse
        |> function
            | true, n when n > 0 -> Ok n
            | true, n -> Error "number must be greater than 0"
            | false, _ -> Error "Input a positive integer"

let showVexValidatedPrompt (placeHolder : string) 
                           (validator : string -> Result<'T, string>) 
                           (callBack : 'T -> unit) 
                           (htmlMessage : string) 
                           (callBack2 : Unit) =
    let cb (s : string) =
        match (unbox s) with
        | false -> ()
        | _ ->
            validator s
            |> function
                | Ok valid -> 
                    callBack valid
                    callBack2
                | Error _ -> 
                    callBack2
    showVexPrompt placeHolder cb htmlMessage

let closeTabDialog (fileName : string option, settingTab : int option, currentId)
                   dispatch = 
    let showFileName =
        match fileName with
        | Some x -> 
            match settingTab with
            | Some y when y = currentId -> "Settings"
            | _ -> x
        | _ -> "Untitled.s"
    let dialog =
        Browser.window.confirm (
            sprintf "You have unsaved changes, are you sure you want to close %s?"
                    (showFileName))
    match dialog with
    | true -> DeleteTab |> dispatch
    | _ -> CloseDialog |> dispatch
    
let alertDialog txt dispatch = 
    showVexAlert txt (fun _ -> CloseDialog |> dispatch )

let showMessage (callBack : bool -> unit) message =
    showVexConfirm message callBack

let close() = electron.ipcRenderer.send "doClose" |> ignore

/// Display dialog asking for confirmation as there is unsaved file(s)
let ExitIfOK dispatch =
    //let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result : bool) =
        match result with
        | false -> CloseDialog |> dispatch
        | true -> Exit |> dispatch
    let mess = "You have unsaved changes. Are you sure you want to exit and lose changes?"
    showMessage callback mess

let ResetIfOK dispatch str msg =
    //let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result : bool) =
        match result with
        | false -> 
            CloseDialog |> dispatch
        | true -> 
            CloseDialog |> dispatch
            ResetEmulator |> dispatch
            msg |> dispatch
    showMessage callback str

/// determine if any dialog box has to be opened in view
let dialogBox (currentFilePath, info, settingTab : int option, dialogBox : DialogBox option)
              dispatch =
    function
   | true -> ()
   | _ ->
        match dialogBox with
        | Option.None -> ()
        | _ ->
            DialogUpdated |> dispatch
            match dialogBox with
            | Some OpenFileDl ->
                openFile currentFilePath 
                         dispatch
            | Some SaveAsDl ->
                saveFileAs currentFilePath 
                           info.Editors.[info.TabId]
                           dispatch
            | Some UnsavedFileDl ->
                closeTabDialog (info.Editors.[info.TabId].FileName, settingTab, info.TabId)
                               dispatch
            | Some QuitDl ->
                ExitIfOK dispatch
            | Some (Alert txt) ->
                alertDialog txt dispatch
            | Some StepBackDl ->
                showVexValidatedPrompt 
                    "steps back" 
                    validPosInt 
                    (fun x -> 
                        CloseDialog |> dispatch
                        x |> int64 |> StepCodeBackBy |> dispatch) 
                    "Number of steps back"
                    (CloseDialog |> dispatch)
            | Some StepDl ->
                showVexValidatedPrompt 
                    "steps forward" 
                    validPosInt 
                    (fun x -> 
                        CloseDialog |> dispatch
                        (ExecutionTop.NoBreak, (int64 x)) |> RunEditorTab |> dispatch) 
                    "Number of steps forward" 
                    (CloseDialog |> dispatch)
            | Some (ResetEmulatorDl (txt, msg)) ->
                ResetIfOK dispatch txt msg
            | Option.None -> 
                ()
            


let attemptToExitUpdate editors =
    let allSaved = 
        editors 
        |> Map.forall (fun _ value -> value.Saved = true) 
    match allSaved with
    | true -> Cmd.ofMsg Exit
    | _ -> QuitDl |> UpdateDialogBox |> Cmd.ofMsg

let maxSymbolWidth = 30
let maxDataSymbolLength = 16

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

let statusBar runMode = 
    let className, txt = 
        match runMode with
        | ExecutionTop.ParseErrorMode -> "btn-negative", "Errors in Code"
        | ExecutionTop.RunErrorMode _ -> "btn-negative", "Runtime Error"
        | ExecutionTop.ResetMode -> "", "-"
        | ExecutionTop.ActiveMode(_, _) -> "btn-primary", "Stepping"
        | ExecutionTop.FinishedMode _ -> "btn-positive", "Execution Complete"
    button [ "btn btn-large btn-default status-bar " + className |> ClassName 
             Disabled true ]
           [ str txt ]

let runButtonText =
    function
    | ExecutionTop.ActiveMode(ExecutionTop.Running, _) -> "Pause"
    | _ -> "Run"

let clockText (n : uint64, m : uint64) =
    match n with
    | 0uL -> "-" 
    | _ -> sprintf "%d : %d" n m
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
        tooltips ((rep |> repTooltipStr |> Refs.Content) :: 
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
        tooltips ((view |> viewTooltipStr |> Refs.Content) :: Placement "bottom" :: defaultTooltipsPropsLst)
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
    
let nameSquash maxW name =
    let nameLen = String.length name
    if nameLen <= maxW then name
    else
        let fp = (float maxW) * 0.65 |> int
        let lp = maxW - (fp + 3)
        name.[0..fp - 1] + "..." + name.[nameLen - lp..nameLen - 1]

let symbolView (symbolMap : Map<string,(uint32 * SymbolType)>) currentRep =
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

/// Converts a memory map to a list of lists which are contiguous blocks of memory
let contiguousMemory reverse (mem : Map<uint32, uint32>) =
    Map.toList mem
    |> List.fold (fun state (addr, value) ->
        match state with
        | [] -> [ [ (addr, value) ] ]
        | hd :: tl ->
            match hd with
            | [] -> failwithf "Contiguous memory never starts a new list with no elements"
            | hd' :: _ when fst hd' = addr - 4u ->
                ((addr, value) :: hd) :: tl // Add to current contiguous block
                           | _ :: _ -> [ (addr, value) ] :: state // Non-contiguous, add to new block
    ) []
    |> List.map (if reverse then id else List.rev) // Reverse each list to go back to increasing
    |> if reverse then id else List.rev // Reverse the overall list

/// Converts a list of (uint32 * uint32) to a byte addressed
/// memory list of (uint32 * uint32) which is 4 times longer
/// LITTLE ENDIAN
let lstToBytes (lst : (uint32 * uint32) list) =
    let byteInfo (dat : uint32) =
        let b = dat &&& 0xFFu
        match b with
        | _ when b >= 32u && b <= 126u -> sprintf "'%c'" (char b), b
        | _ -> "", b
    lst
    |> List.collect (fun (addr, value) ->
        [
            addr, value |> byteInfo
            addr + 1u, (value >>> 8) |> byteInfo
            addr + 2u, (value >>> 16) |> byteInfo
            addr + 3u, (value >>> 24) |> byteInfo
        ]
    )

let memView memoryMap dispatch byteView reverseView runMode currentRep symbolMap = 
    let stackInfo =
        match runMode with
        | FinishedMode ri
        | RunErrorMode ri
        | ActiveMode(_, ri) ->
            let sp = (fst ri.dpCurrent).Regs.[CommonData.R13]
            Some(ri.StackInfo, sp)
        | _ -> Core.Option.None
    let chWidth = 13
    let memPanelShim = 50
    let onlyIfByte x = if byteView then [ x ] else []
    let invSymbolTypeMap symType =
        symbolMap
        |> Map.toList
        |> List.filter (fun (_, (_, typ)) -> typ = symType)
        |> List.distinctBy (fun (_, (addr, _)) -> addr)
        |> List.map (fun (sym, (addr, _)) -> (addr, sym))
        |> Map.ofList
    let invSymbolMap = invSymbolTypeMap ExecutionTop.DataSymbol
    let invCodeMap = invSymbolTypeMap ExecutionTop.CodeSymbol
    let invStackMap =
        match stackInfo with
        | Some(si, sp) -> si |> List.map (fun { SP = sp; Target = target } ->
                                        sp - 4u, match Map.tryFind target invCodeMap with
                                                 | Some s -> "(" + s + ")"
                                                 | None -> sprintf "(%08x)" target)
                           |> Map.ofList, sp
        | _ -> Map.empty, 0u
        |> (fun (map, sp) -> // add SP legend
                let lab =
                    match sp, Map.tryFind sp map with
                    | 0u, Some sym -> sym
                    | 0u, None -> ""
                    | _, None -> "SP ->"
                    | _, Some sym -> sym + " ->"
                Map.add sp lab map)

    let lookupSym addr =
            match Map.tryFind addr invSymbolMap, Map.tryFind addr invStackMap with
            | Some sym, _ -> sym
            | option.None, Some sub -> sub
            | _ -> ""

    let makeRow (addr : uint32, (chRep : string, value : uint32)) =

        let rowDat =
            [
                lookupSym addr |> nameSquash maxDataSymbolLength
                sprintf "0x%X" addr
                (if byteView then
                    formatterWithWidth 8 currentRep value +
                    (chRep |> function | "" -> "" | chr -> sprintf " %s" chr)
                else formatter currentRep value)
            ]

        let makeNode txt = td [ ClassName "selectable-text" ] [ str txt ]

        tr [ ClassName "tr-head-mem" ] (List.map makeNode rowDat)

    let makeContig (lst : (uint32 * uint32) list) =

        let makeNode txt = th [ ClassName "th-mem" ] [ str txt ]

        let tr = tr [] 
                    (List.map makeNode ([ "Symbol"; "Address"; "Value" ]))

        let byteSwitcher =
            match byteView with
            | true -> lstToBytes
            | false -> List.map (fun (addr, dat) -> (addr, ("", dat)))

        // Add each row to the table from lst
        let rows =
            lst
            |> byteSwitcher
            |> List.map makeRow

        li [ ClassName "list-group-item"
             Style [ Padding 0 ] ]
           [ table [ ClassName "table-striped" ]
           ([ tr ] @ rows) ]
           
    ul [ ClassName "list-group" ]
       [ li [ Class "list-group-item" ]
            [ div [ Class "btn-group full-width" ]
                  [ button [ viewButtonClass byteView
                             DOMAttr.OnClick (fun _ -> ToggleByteView |> dispatch)]
                           [ byteViewButtonString byteView ]
                    button [ viewButtonClass reverseView
                             DOMAttr.OnClick (fun _ -> ToggleReverseView |> dispatch)]
                           [ reverseDirectionButtonString reverseView ] ] ]
         li [ ClassName "list-group-item"
              Style [ Padding 0 ] ]
            (memoryMap
             |> contiguousMemory reverseView
             |> List.map makeContig ) 
         li [ ]
            [ div [ Style [ TextAlign "center" ] ]
                  [ b [] [ str "Uninitialized memory is zeroed" ] ] ] ] 


let regView (regMap : Map<CommonData.RName, uint32>) currentRep = 
    let registerLi rName  = 
        li [ ClassName "list-group-item" ] 
           [ div [ ClassName "btn-group full-width" ] 
                 [ tooltips ((rName |> regTooltipStr |> Refs.Content) :: Placement "bottom" :: basicTooltipsPropsLst)
                            [ button [ ClassName "btn btn-reg" ] 
                                     [  rName |> string |> str ] ]
                   span [ ClassName "btn btn-reg-con selectable-text" ] 
                        [ regMap.[rName] |> formatter currentRep |> str ] ] ]
    let registerSet regMap =
        [0 .. 15]
        |> List.map (fun x -> 
            x |> CommonData.register |> registerLi )

    ul [ ClassName "list-group" ] (registerSet regMap)



let footerColour =
    function
    | false -> "#fcfcfc"
    | _ -> "#4285f4"

let footer (flags : CommonData.Flags) (flagsChanged : bool) =
    let footerDiv (letter) (flag : bool)=
        let text =
            match flag with
            | true -> "1"
            | false -> "0"
        div [ ClassName "btn-group btn-flag-group" 
              Style [ Margin 5 ] ] 
            [ button [ ClassName "btn btn-flag" ] 
                     [ str letter ]
              button [ ClassName "btn btn-flag-con" 
                       Style [ Background (flagsChanged |> footerColour) ] ] 
                     [ str text ] ]
    footer [ ClassName "toolbar toolbar-footer" ] 
           [ tooltips (Refs.Content flagTooltipStr :: Placement "top" :: basicTooltipsPropsLst)
                      [ div [ ClassName "pull-right" ]
                            [ footerDiv "N" flags.N
                              footerDiv "Z" flags.Z
                              footerDiv "C" flags.C
                              footerDiv "V" flags.V ] ] ]

/// return the view panel on the right
let viewPanel (view, content, runMode)
              dispatch = 
    let viewEl =
        match view.CurrentView with
        | Registers -> regView content.RegMap view.CurrentRep
        | Memory -> memView content.MemoryMap dispatch view.ByteView view.ReverseDirection runMode view.CurrentRep view.SymbolMap
        | Symbols -> symbolView view.SymbolMap view.CurrentRep
    div [ ClassName "pane dashboard"
          dashboardStyle view.CurrentRep ]
        [ viewButtons view.CurrentView dispatch
          div [ ClassName "viewer" ; viewerStyle view.CurrentRep ] 
              [ viewEl ]
          footer content.Flags view.FlagsHaveChanged ]