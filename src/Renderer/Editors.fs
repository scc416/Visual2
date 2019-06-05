(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Editors
    Description: Interface with Monaco editor buffers
*)

/// Interface with monaco editor buffers
module Editors

open Fable.Core.JsInterop
open Fable.Import
open Fable.Core
open EEExtensions
open Refs
open Tooltips
open Elmish
open Fable.Helpers.React
open Monaco
open Fable.Helpers.React.Props
open Fable.Import.React
open Settings

open CommonData
open Memory

[<Emit "new monaco.Range($0,$1,$2,$3)">]
let monacoRange _ _ _ _ = jsNative

[<Emit "$0.deltaDecorations($1, [
    { range: $2, options: $3},
  ]);">]
let lineDecoration _editor _decorations _range _name = jsNative

[<Emit "$0.deltaDecorations($1, [{ range: new monaco.Range(1,1,1,1), options : { } }]);">]
let removeDecorations _editor _decorations =
    jsNative

let editorLineDecorate editor number decoration (rangeOpt : (int * int) option) decorations =
    let model = editor?getModel ()
    let lineWidth = model?getLineMaxColumn (number)
    let posStart = match rangeOpt with | None -> 1 | Some(n, _) -> n
    let posEnd = match rangeOpt with | None -> lineWidth | Some(_, n) -> n
    let newDecs = lineDecoration editor
                    decorations
                    (monacoRange number posStart number posEnd)
                    decoration
    List.append decorations [ newDecs ]

let highlightGlyph editor number glyphClassName decorations =
    editorLineDecorate
        editor
        number
        (createObj [
            "isWholeLine" ==> true
            "glyphMarginClassName" ==> glyphClassName
        ])
        None
        decorations

let highlightNextInstruction editor number decorations =
    if number > 0 
        then highlightGlyph editor number "editor-glyph-margin-arrow" decorations
        else decorations

let revealLineInWindow editor (lineNumber : int) =
    editor?revealLineInCenterIfOutsideViewport (lineNumber) |> ignore

// highlight a particular line
let highlightLine editor number className decorations =
    editorLineDecorate
        editor
        number
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> className
        ])
        None
        decorations

/// <summary>
/// Decorate a line with an error indication and set up a hover message.
/// Distinct message lines must be elements of markdownLst.
/// markdownLst: string list - list of markdown paragraphs.
/// tId: int - tab identifier.
/// lineNumber: int - line to decorate, starting at 1.
/// hoverLst: hover attached to line.
/// gHoverLst: hover attached to margin glyph.</summary>
let makeErrorInEditor lineNumber (hoverLst : string list) (gHoverLst : string list) (editor: Monaco.Editor.IEditor option) decorations =
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt -> createObj [ "isTrusted" ==> true; "value" ==> txt ])
    // decorate the line and margin
    editorLineDecorate 
        editor
        lineNumber
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> "editor-line-error"
            "hoverMessage" ==> makeMarkDown hoverLst
            "glyphMarginClassName" ==> "editor-glyph-margin-error"
            "glyphMarginHoverMessage" ==> makeMarkDown gHoverLst
            "overviewRuler" ==> createObj [ "position" ==> 4 ]
         ])
        None
        decorations




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
    ]



let inline editor (props: EditorsProps list) : React.ReactElement =
    ofImport "default" "react-monaco-editor" (keyValueList CaseRules.LowerFirst props) []
 
// ***********************************************************************************
//                     Functions Relating to Editor Panel
// ***********************************************************************************

let tabClickable id dispatch =
    function
    | true -> (SelectFileTab id, "select tab") |> CheckRunMode |> dispatch
    | _ -> ()

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
    | Some x when x = id -> "tab-file-name icon icon-cog" 
    | _ -> "tab-file-name" 

let tabGroupClass =
    function
    | true -> "tab-group tabs-files"
    | _ -> "tab-group tabs-files disabled-click"

let tabGroupOnClick (dispatch : Msg -> unit) =
    function
    | true -> ()
    | _ -> ("Cannot change tabs during execution") |> Alert |> UpdateDialogBox |> dispatch

let overlayClass =
    function
    | true -> "invisible"
    | _ -> "darken-overlay disabled-click"

/// return all the react elements in the editor panel (including the react monaco editor)
let editorPanel (info, settingsTabId, settings, editorEnable) 
                dispatch =

    /// return the class of the tab header
    /// "active" if it is the current tab
    let tabHeaderClass (id : int) : HTMLAttr =
        match id with
        | x when x = info.TabId -> ClassName "tab-item tab-file active" 
        | _ -> ClassName "tab-item tab-file"   

    let editorClass (id : int) : HTMLAttr =
        match id with
        | x when x = info.TabId -> ClassName "editor" 
        | _ -> ClassName "editor invisible"

    let editor txt id = 
        let onChange : EditorsProps = 
            Refs.OnChange (fun _ -> 
                match info.Editors.[info.TabId].Saved with
                | true -> EditorTextChange |> dispatch
                | false -> ())
        editor [ onChange
                 settings |> editorOptions |> Options 
                 Refs.DefaultValue txt 
                 EditorWillMount (fun x -> InitialiseIExports x |> dispatch)
                 EditorDidMount (fun x -> UpdateIEditor (x, id) |> dispatch) ]

    let editorDiv id =
        div [ editorClass id
              id |> tabNameIdFormatter |> Key ] 
            [ editor info.Editors.[info.TabId].DefaultValue id ]

    /// return a tab header
    let tabHeaderDiv (id : int) (editor : Editor) : ReactElement =

        /// return "untitled.s" if the tab has no filename
        let fileName =
            match editor.FileName with
            | Some x -> x
            | _ -> "Untitled.s"

        div [ tabHeaderClass id
              DOMAttr.OnClick (fun _ -> tabClickable id dispatch editorEnable)] 
            [ span [ ClassName "invisible" ] []
              span [ ClassName "icon icon-cancel icon-close-tab" 
                     DOMAttr.OnClick (fun _ -> (AttemptToDeleteTab id, "close file") |> CheckRunMode |> dispatch)] []
              span [ tabNameClass id settingsTabId |> ClassName
                     tabHeaderTextStyle editor.Saved ] 
                   [ editor.Saved |> fileNameFormat fileName |> str ] ]

    /// button (actually is a clickable div) that add new tab
    let addNewTabDiv =
        [ div [ ClassName "tab-item tab-item-fixed" 
                DOMAttr.OnClick (fun _ -> (NewFile, "make new file tab")|> CheckRunMode |> dispatch) ]
              [ span [ ClassName "icon icon-plus" ] [] ] ]

    /// all tab headers plus the add new tab button
    let tabHeaders =
        let filesHeader = 
            info.Editors
            |> Map.map (fun id editor -> tabHeaderDiv id editor)
            |> Map.toList
            |> List.map (fun (_, name) -> name)

        addNewTabDiv 
        |> List.append filesHeader
        |> div [ editorEnable |> tabGroupClass |> ClassName 
                 DOMAttr.OnClick (fun _ -> tabGroupOnClick dispatch editorEnable) ]
        
    /// the editor
    let editorViewDiv =
        let editorsLst = 
            info.Editors
            |> Map.toList
            |> List.map (fun (key, _) -> editorDiv key)
        match settingsTabId with
        | Some x when x = info.TabId -> 
            let settingEl = settingsMenu dispatch settings info.Editors.[info.TabId].Saved
            settingEl :: editorsLst
        | _ -> 
            editorsLst
    tabHeaders :: editorViewDiv

/// find editor Horizontal char position after end of code (ignoring comment)
let findCodeEnd (lineCol : int) editor =
    let tabSize = 6
    let txt =  formatText editor
    if txt.Length <= lineCol then
        0
    else
        let line = txt.[lineCol]
        match String.splitRemoveEmptyEntries [| ';' |] line |> Array.toList with
        | s :: _ -> (s.Length / tabSize) * tabSize + (if s.Length % tabSize > 0 then tabSize else 0)
        | [] -> 0



/// Make an editor tooltip info button with correct theme
let makeEditorInfoButton clickable (h, v, orientation) editorTheme = 
    makeEditorInfoButtonWithTheme (tippyTheme editorTheme) clickable (h, v, orientation)

let lineTipsClickable = false

/// Make execution tooltip info for the given instruction and line v, dp before instruction dp.
/// Does nothing if opcode is not documented with execution tooltip
let toolTipInfo (v : int, orientation : string)
                (dp : DataPath)
                ({ Cond = cond; InsExec = instruction; InsOpCode = opc } : ParseTop.CondInstr)
                editor 
                editorTheme : Cmd<Msg> =
    match Helpers.condExecute cond dp, instruction with
    | false, _ -> Cmd.none
    | true, ParseTop.IMEM ins ->
        match Memory.executeMem ins dp with
        | Error _ -> Cmd.none
        | Ok res ->
            let TROWS s =
                (List.map (fun s -> s |> toDOM |> TD) >> TROW) s
            let memStackInfo (ins : Memory.InstrMemMult) (dir : MemDirection) (dp : DataPath) =
                let sp = dp.Regs.[ins.Rn]
                let offLst, increment = Memory.offsetList (sp |> int32) ins.suff ins.rList ins.WB (dir = MemRead)
                let locs = List.zip ins.rList offLst
                let makeRegRow (rn : RName, ol : uint32) =
                    [
                        rn.ToString()
                        (match dir with | MemRead -> "\u2190" | MemWrite -> "\u2192")
                        (sprintf "Mem<sub>32</sub>[0x%08X]" ol)
                        (match dir with
                            | MemRead -> Map.tryFind (WA ol) dp.MM |> (function | Some(Dat x) -> x | _ -> 0u)
                            | MemWrite -> dp.Regs.[rn])
                        |> (fun x -> if abs (int x) < 10000 then sprintf "(%d)" x else sprintf "(0x%08X)" x)
                    ]
                let regRows =
                    locs
                    |> List.map (makeRegRow >> TROWS)
                (findCodeEnd v editor, "Stack"), TABLE [] [
                    DIV [] [
                        TROWS [ sprintf "Pointer (%s)" (ins.Rn.ToString()); sprintf "0x%08X" sp ]
                        TROWS [ "Increment"; increment |> sprintf "%d" ]
                    ]
                    DIV [ "tooltip-stack-regs-" + tippyTheme editorTheme + "-theme" ] regRows ]


            let memPointerInfo (ins : Memory.InstrMemSingle) (dir : MemDirection) (dp : DataPath) =
                let baseAddrU =
                    let rb = dp.Regs.[ins.Rb]
                    match ins.Rb with | R15 -> rb + 8u | _ -> rb
                let baseAddr = int32 baseAddrU
                let offset = (ins.MAddr dp baseAddr |> uint32) - baseAddrU |> int32
                let ea = match ins.MemMode with | Memory.PreIndex | Memory.NoIndex -> (baseAddrU + uint32 offset) | _ -> baseAddrU
                let mData = (match ins.MemSize with | MWord -> Memory.getDataMemWord | MByte -> Memory.getDataMemByte) ea dp
                let isIncr = match ins.MemMode with | Memory.NoIndex -> false | _ -> true
                (findCodeEnd v editor, "Pointer"), TABLE [] [
                    TROWS [ sprintf "Base (%s)" (ins.Rb.ToString()); sprintf "0x%08X" baseAddrU ]
                    TROWS [ "Address"; ea |> sprintf "0x%08X" ]
                    TROWS <| if isIncr then [] else [ "Offset"; (offset |> sprintf "%+d") ]
                    TROWS [ "Increment"; (if isIncr then offset else 0) |> (fun n -> sprintf "%+d" n) ]
                    TROWS [ "Data"; match ins.LSType with
                                    | LOAD -> match mData with | Ok dat -> dat | _ -> 0u
                                    | STORE -> dp.Regs.[ins.Rd]
                                    |> fun d ->
                                        match ins.MemSize with
                                        | MWord -> sprintf "0x%08X" d
                                        | MByte -> sprintf "0x%02X" ((uint32 d) % 256u) ]
                    ]

            let cmd = 
                    [ Cmd.ofMsg DeleteAllContentWidgets 
                      Cmd.ofMsg DeleteAllContentWidgets ]
            let makeTip memInfo =
                let (hOffset, label), tipDom = memInfo dp
                [ ((tippyTheme editorTheme), Tooltips.lineTipsClickable, hOffset, (v + 1), orientation, tipDom, label) 
                |> MakeEditorInfoButton 
                |> Cmd.ofMsg ]
                |> List.append cmd
                |> Cmd.batch
            match ins with
            | Memory.LDR ins -> makeTip <| memPointerInfo ins MemRead
            | Memory.STR ins -> makeTip <| memPointerInfo ins MemWrite
            | Memory.LDM ins -> makeTip <| memStackInfo ins MemRead
            | Memory.STM ins -> makeTip <| memStackInfo ins MemWrite
            | _ -> Cmd.none
    | true, ParseTop.IDP(exec, op2) ->
        let alu = ExecutionTop.isArithmeticOpCode opc
        let pos1, pos2, pos3 = findCodeEnd v editor, v, orientation
        match exec dp with
        | Error _ -> Cmd.none
        | Ok(dp', uF') ->
            match op2 with
            | DP.Op2.NumberLiteral _
            | DP.Op2.RegisterWithShift(_, _, 0u) -> Cmd.none
            | DP.Op2.RegisterWithShift(rn, shiftT, shiftAmt) ->
                (pos1, pos2, pos3, dp, dp', uF', rn, Some shiftT, alu, shiftAmt, op2) |> MakeShiftTooltip |> Cmd.ofMsg
            | DP.Op2.RegisterWithRegisterShift(rn, shiftT, sRn) ->
                (pos1, pos2, pos3, dp, dp', uF', rn, Some shiftT, alu, (dp.Regs.[sRn] % 32u), op2) |> MakeShiftTooltip |> Cmd.ofMsg
            | DP.Op2.RegisterWithRRX rn -> 
                (pos1, pos2, pos3, dp, dp', uF', rn, None, alu, 1u, op2) |> MakeShiftTooltip |> Cmd.ofMsg
    | _ -> Cmd.none

