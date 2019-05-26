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
open Fable.Import.Browser
open Fable.Core
open EEExtensions
open Refs
open Tooltips

open CommonData
open Memory

let editorOptions (readOnly : bool) =
    let vs = Refs.vSettings
    createObj [

                        // User defined settings
                        "theme" ==> vs.EditorTheme
                        "renderWhitespace" ==> vs.EditorRenderWhitespace
                        "fontSize" ==> vs.EditorFontSize
                        "wordWrap" ==> vs.EditorWordWrap

                        // Application defined settings
                        "value" ==> "";
                        "renderIndentGuides" ==> false
                        "fontFamily" ==> "fira-code"
                        "fontWeight" ==> "bold"
                        "language" ==> "arm";
                        "roundedSelection" ==> false;
                        "scrollBeyondLastLine" ==> false;
                        "readOnly" ==> readOnly;
                        "automaticLayout" ==> true;
                        "minimap" ==> createObj [ "enabled" ==> false ];
                        "glyphMargin" ==> true
              ]


let updateEditor tId readOnly =
    if tId <> -1 then
        let eo = editorOptions readOnly
        Refs.editors.[tId]?updateOptions (eo) |> ignore

let setTheme theme =
    window?monaco?editor?setTheme (theme)


let updateAllEditors readOnly =
    Refs.editors
    |> Map.iter (fun tId _ -> if tId = Refs.currentFileTabId then readOnly else false
                              |> updateEditor tId)
    let theme = Refs.vSettings.EditorTheme
    Refs.setFilePaneBackground (
        match theme with
        | "one-light-pro" | "solarised-light" -> "white"
        | _ -> "black")
    setTheme (theme) |> ignore
    setCustomCSS "--editor-font-size" (sprintf "%spx" vSettings.EditorFontSize)


// Disable the editor and tab selection during execution
let disableEditors() =
    Refs.fileTabMenu.classList.add ("disabled-click")
    Refs.fileTabMenu.onclick <- (fun _ ->
        showVexAlert ("Cannot change tabs during execution")
        createObj []
    )
    updateEditor Refs.currentFileTabId true
    Refs.darkenOverlay.classList.remove ("invisible")
    Refs.darkenOverlay.classList.add ([| "disabled-click" |])

// Enable the editor once execution has completed
let enableEditors() =
    Refs.fileTabMenu.classList.remove ("disabled-click")
    Refs.fileTabMenu.onclick <- (fun _ -> createObj [])
    updateEditor Refs.currentFileTabId false
    Refs.darkenOverlay.classList.add ([| "invisible" |])

//let mutable decorations : obj list = []
//let mutable lineDecorations : obj list = []

[<Emit "new monaco.Range($0,$1,$2,$3)">]
let monacoRange _ _ _ _ = jsNative

[<Emit "$0.deltaDecorations($1, [
    { range: $2, options: $3},
  ]);">]
let lineDecoration _editor _decorations _range _name = jsNative

[<Emit "$0.deltaDecorations($1, [{ range: new monaco.Range(1,1,1,1), options : { } }]);">]
let removeDecorations _editor _decorations =
    jsNative

// Remove all text decorations associated with an editor
let removeEditorDecorations tId decorations (editors : Map<int, Editor>) : obj list =
    if tId <> -1 then
        List.iter (fun x -> removeDecorations editors.[tId].IEditor x) decorations
        []
        else decorations

let editorLineDecorate editor number decoration (rangeOpt : (int * int) option) (m : Model) =
    Browser.console.log("margin decoration")
    Browser.console.log(string m.Decorations)
    let model = editor?getModel ()
    let lineWidth = model?getLineMaxColumn (number)
    let posStart = match rangeOpt with | None -> 1 | Some(n, _) -> n
    let posEnd = match rangeOpt with | None -> lineWidth | Some(_, n) -> n
    let newDecs = lineDecoration editor
                    m.Decorations
                    (monacoRange number posStart number posEnd)
                    decoration
    { m with Decorations = List.append m.Decorations [ newDecs ] }

// highlight a particular line
let highlightLine tId (editors : Map<int, Editor>) number className (m : Model) =
    Browser.console.log("EDITOR 124")
    editorLineDecorate
        editors.[tId].IEditor
        number
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> className
        ])
        None
        m
let highlightGlyph number glyphClassName (m : Model) =
    Browser.console.log("EDITOR 136")
    editorLineDecorate
        m.Editors.[m.CurrentFileTabId].IEditor
        number
        (createObj [
            "isWholeLine" ==> true
            "glyphMarginClassName" ==> glyphClassName
        ])
        None
        m

let highlightNextInstruction number (m : Model) =
    if number > 0 then highlightGlyph number "editor-glyph-margin-arrow" m
        else m

/// <summary>
/// Decorate a line with an error indication and set up a hover message.
/// Distinct message lines must be elements of markdownLst.
/// markdownLst: string list - list of markdown paragraphs.
/// tId: int - tab identifier.
/// lineNumber: int - line to decorate, starting at 1.
/// hoverLst: hover attached to line.
/// gHoverLst: hover attached to margin glyph.</summary>
let makeErrorInEditor tId lineNumber (hoverLst : string list) (gHoverLst : string list) (m : Model)=
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt -> createObj [ "isTrusted" ==> true; "value" ==> txt ])
    // decorate the line
    let m2 = 
        Browser.console.log("editor 166")
        editorLineDecorate
            m.Editors.[tId].IEditor
            lineNumber
            (createObj [
                "isWholeLine" ==> true
                "isTrusted" ==> true
                "inlineClassName" ==> "editor-line-error"
                "hoverMessage" ==> makeMarkDown hoverLst
             ])
            None
            m
    // decorate the margin
    Browser.console.log("editor 179")
    editorLineDecorate
        m2.Editors.[tId].IEditor
        lineNumber
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "glyphMarginClassName" ==> "editor-glyph-margin-error"
            "glyphMarginHoverMessage" ==> makeMarkDown gHoverLst
            "overviewRuler" ==> createObj [ "position" ==> 4 ]
        ])
        None
        m2

let revealLineInWindow tId (lineNumber : int) (editors : Map<int, Editor>) =
    editors.[tId].IEditor?revealLineInCenterIfOutsideViewport (lineNumber) |> ignore

//*************************************************************************************
//                              EDITOR CONTENT WIDGETS
//*************************************************************************************

type MemDirection = | MemRead | MemWrite

/// find editor Horizontal char position after end of code (ignoring comment)
let findCodeEnd (lineCol : int) tId (editors: Map<int, Editor>) =
    let tabSize = 6
    match tId with
    | -1 -> 0
    | x ->
        let text = formatText tId editors
        if text.Length <= lineCol then
            0
        else
            let line = text.[lineCol]
            match String.splitRemoveEmptyEntries [| ';' |] line |> Array.toList with
            | s :: _ -> (s.Length / tabSize) * tabSize + (if s.Length % tabSize > 0 then tabSize else 0)
            | [] -> 0


/// Make execution tooltip info for the given instruction and line v, dp before instruction dp.
/// Does nothing if opcode is not documented with execution tooltip
let toolTipInfo (v : int, orientation : string)
                (dp : DataPath)
                ({ Cond = cond; InsExec = instruction; InsOpCode = opc } : ParseTop.CondInstr) 
                (m : Model) : Model =
    match Helpers.condExecute cond dp, instruction with
    | false, _ -> m
    | true, ParseTop.IMEM ins ->
        match Memory.executeMem ins dp with
        | Error _ -> m
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
                (findCodeEnd v m.CurrentFileTabId m.Editors, "Stack"), TABLE [] [ //TODO: HTML ELEMENT MIGHT NOT WORK
                    DIV [] [
                        TROWS [ sprintf "Pointer (%s)" (ins.Rn.ToString()); sprintf "0x%08X" sp ]
                        TROWS [ "Increment"; increment |> sprintf "%d" ]
                    ]
                    DIV [ "tooltip-stack-regs-" + tippyTheme m.Settings.EditorTheme + "-theme" ] regRows ]


            let memPointerInfo (ins : Memory.InstrMemSingle) (dir : MemDirection) (dp : DataPath) =
                let baseAddrU =
                    let rb = dp.Regs.[ins.Rb]
                    match ins.Rb with | R15 -> rb + 8u | _ -> rb
                let baseAddr = int32 baseAddrU
                let offset = (ins.MAddr dp baseAddr |> uint32) - baseAddrU |> int32
                let ea = match ins.MemMode with | Memory.PreIndex | Memory.NoIndex -> (baseAddrU + uint32 offset) | _ -> baseAddrU
                let mData = (match ins.MemSize with | MWord -> Memory.getDataMemWord | MByte -> Memory.getDataMemByte) ea dp
                let isIncr = match ins.MemMode with | Memory.NoIndex -> false | _ -> true
                (findCodeEnd v m.CurrentFileTabId m.Editors, "Pointer"), TABLE [] [
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

            let makeTip memInfo =
                let (hOffset, label), tipDom = memInfo dp
                makeEditorInfoButton Tooltips.lineTipsClickable 
                                     m.Settings.EditorTheme
                                     (hOffset, (v + 1), orientation) 
                                     label 
                                     tipDom 
                                     m
            match ins with
            | Memory.LDR ins -> makeTip <| memPointerInfo ins MemRead
            | Memory.STR ins -> makeTip <| memPointerInfo ins MemWrite
            | Memory.LDM ins -> makeTip <| memStackInfo ins MemRead
            | Memory.STM ins -> makeTip <| memStackInfo ins MemWrite
            | _ -> m
    | true, ParseTop.IDP(exec, op2) ->
        let alu = ExecutionTop.isArithmeticOpCode opc
        let pos = findCodeEnd v m.CurrentFileTabId m.Editors, v, orientation
        match exec dp with
        | Error _ -> m
        | Ok(dp', uF') ->
            match op2 with
            | DP.Op2.NumberLiteral _
            | DP.Op2.RegisterWithShift(_, _, 0u) -> m
            | DP.Op2.RegisterWithShift(rn, shiftT, shiftAmt) ->
                    makeShiftTooltip pos (dp, dp', uF') rn (Some shiftT, alu) shiftAmt op2 m
                    
            | DP.Op2.RegisterWithRegisterShift(rn, shiftT, sRn) ->
                    makeShiftTooltip pos (dp, dp', uF') rn (Some shiftT, alu) (dp.Regs.[sRn] % 32u) op2 m

            | DP.Op2.RegisterWithRRX rn -> makeShiftTooltip pos (dp, dp', uF') rn (None, alu) 1u op2 m
    | _ -> m
