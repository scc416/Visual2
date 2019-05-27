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
open Settings

open CommonData
open Memory

let setTheme theme =
    window?monaco?editor?setTheme (theme)

// Disable the editor and tab selection during execution
//let disableEditors() =
    //Refs.fileTabMenu.classList.add ("disabled-click")
    //Refs.fileTabMenu.onclick <- (fun _ ->
    //    showVexAlert ("Cannot change tabs during execution")
    //    createObj []
    //)
    //Refs.darkenOverlay.classList.remove ("invisible")
    //Refs.darkenOverlay.classList.add ([| "disabled-click" |])

// Enable the editor once execution has completed
//let enableEditors() =
    //Refs.fileTabMenu.classList.remove ("disabled-click")
    //Refs.fileTabMenu.onclick <- (fun _ -> createObj [])
    //Refs.darkenOverlay.classList.add ([| "invisible" |])

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

let editorLineDecorate editor number decoration (rangeOpt : (int * int) option) (m : Model) : obj list =
    Browser.console.log("margin decoration")
    Browser.console.log(string m.Decorations)
    let model = editor?getModel ()
    Browser.console.log(string model)
    let lineWidth = model?getLineMaxColumn (number)
    let posStart = match rangeOpt with | None -> 1 | Some(n, _) -> n
    let posEnd = match rangeOpt with | None -> lineWidth | Some(_, n) -> n
    let newDecs = lineDecoration editor
                    m.Decorations
                    (monacoRange number posStart number posEnd)
                    decoration
    Browser.console.log(string newDecs)
    List.append m.Decorations [ newDecs ]

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
        m.Editors.[m.TabId].IEditor
        number
        (createObj [
            "isWholeLine" ==> true
            "glyphMarginClassName" ==> glyphClassName
        ])
        None
        m

let highlightNextInstruction number (m : Model) =
    if number > 0 then highlightGlyph number "editor-glyph-margin-arrow" m
        else m.Decorations

/// <summary>
/// Decorate a line with an error indication and set up a hover message.
/// Distinct message lines must be elements of markdownLst.
/// markdownLst: string list - list of markdown paragraphs.
/// tId: int - tab identifier.
/// lineNumber: int - line to decorate, starting at 1.
/// hoverLst: hover attached to line.
/// gHoverLst: hover attached to margin glyph.</summary>
let makeErrorInEditor tId lineNumber (hoverLst : string list) (gHoverLst : string list) (m : Model) =
    let makeMarkDown (textLst : string list) =
        Browser.console.log("making tooltips")
        textLst
        |> List.toArray
        |> Array.map (fun txt -> createObj [ "isTrusted" ==> true; "value" ==> txt ])
    // decorate the line
    let decorations2 = 
        Browser.console.log("editor 166")
        editorLineDecorate
            m.Editors.[tId].IEditor
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
            m
    // decorate the margin
    //let decorations3 =
        //Browser.console.log("editor 179")
        //editorLineDecorate
            //m.Editors.[tId].IEditor
            //lineNumber
            //(createObj [
            //    "isWholeLine" ==> true
            //    "isTrusted" ==> true

            //])
            //None
            //m

    Browser.console.log("decorations 2")
    Browser.console.log(string decorations2)
    //Browser.console.log("decorations 3")
    //Browser.console.log(string decorations3)
    decorations2 //@ decorations3

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
        let text = formatText editors.[tId].IEditor
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
                (findCodeEnd v m.TabId m.Editors, "Stack"), TABLE [] [ //TODO: HTML ELEMENT MIGHT NOT WORK
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
                (findCodeEnd v m.TabId m.Editors, "Pointer"), TABLE [] [
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
        let pos = findCodeEnd v m.TabId m.Editors, v, orientation
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
                 EditorWillMount (fun x -> InitialiseIExports x |> dispatch)
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

let registerLanguage =
    createObj [
        "id" ==> "arm"
    ]

let keywordsLst = 
    [|

      "ADC";"ADCCS";"ADCEQ";"ADCGE";"ADCGT";"ADCHI";"ADCHS";"ADCLE";"ADCLO";"ADCLS";"ADCLT";"ADCMI";

      "ADCNE";"ADCCC";"ADCPL";"ADCS";"ADCSCS";"ADCSEQ";"ADCSGE";"ADCSGT";"ADCSHI";"ADCSHS";"ADCSLE";

      "ADCSLO";"ADCSLS";"ADCSLT";"ADCSMI";"ADCSNE";"ADCSCC";"ADCSPL";"ADCSVC";"ADCSVS";"ADCVC";"ADCVS";"ADD";

      "ADDCS";"ADDEQ";"ADDGE";"ADDGT";"ADDHI";"ADDHS";"ADDLE";"ADDLO";"ADDLS";"ADDLT";"ADDMI";"ADDNE";"ADDCC";

      "ADDPL";"ADDS";"ADDSCS";"ADDSEQ";"ADDSGE";"ADDSGT";"ADDSHI";"ADDSHS";"ADDSLE";"ADDSLO";"ADDSLS";"ADDSLT";

      "ADDSMI";"ADDSNE";"ADDSCC";"ADDSPL";"ADDSVC";"ADDSVS";"ADDVC";"ADDVS";"AND";"ANDCS";"ANDEQ";"ANDGE";"ANDGT";

      "ANDHI";"ANDHS";"ANDLE";"ANDLO";"ANDLS";"ANDLT";"ANDMI";"ANDNE";"ANDCC";"ANDPL";"ANDS";"ANDSCS";"ANDSEQ";

      "ANDSGE";"ANDSGT";"ANDSHI";"ANDSHS";"ANDSLE";"ANDSLO";"ANDSLS";"ANDSLT";"ANDSMI";"ANDSNE";"ANDSCC";"ANDSPL";

      "ANDSVC";"ANDSVS";"ANDVC";"ANDVS";"ASR";"ASRCS";"ASREQ";"ASRGE";"ASRGT";"ASRHI";"ASRHS";"ASRLE";"ASRLO";

      "ASRLS";"ASRLT";"ASRMI";"ASRNE";"ASRCC";"ASRPL";"ASRS";"ASRSCS";"ASRSEQ";"ASRSGE";"ASRSGT";"ASRSHI";"ASRSHS";

      "ASRSLE";"ASRSLO";"ASRSLS";"ASRSLT";"ASRSMI";"ASRSNE";"ASRSCC";"ASRSPL";"ASRSVC";"ASRSVS";"ASRVC";"ASRVS";

      "BIC";"BICCS";"BICEQ";"BICGE";"BICGT";"BICHI";"BICHS";"BICLE";"BICLO";"BICLS";"BICLT";"BICMI";"BICNE";"BICCC";

      "BICPL";"BICS";"BICSCS";"BICSEQ";"BICSGE";"BICSGT";"BICSHI";"BICSHS";"BICSLE";"BICSLO";"BICSLS";"BICSLT";

      "BICSMI";"BICSNE";"BICSCC";"BICSPL";"BICSVC";"BICSVS";"BICVC";"BICVS";"CMN";"CMNCS";"CMNEQ";"CMNGE";"CMNGT";

      "CMNHI";"CMNHS";"CMNLE";"CMNLO";"CMNLS";"CMNLT";"CMNMI";"CMNNE";"CMNCC";"CMNPL";"CMNS";"CMNSCS";"CMNSEQ";

      "CMNSGE";"CMNSGT";"CMNSHI";"CMNSHS";"CMNSLE";"CMNSLO";"CMNSLS";"CMNSLT";"CMNSMI";"CMNSNE";"CMNSCC";"CMNSPL";

      "CMNSVC";"CMNSVS";"CMNVC";"CMNVS";"CMP";"CMPCS";"CMPEQ";"CMPGE";"CMPGT";"CMPHI";"CMPHS";"CMPLE";"CMPLO";

      "CMPLS";"CMPLT";"CMPMI";"CMPNE";"CMPCC";"CMPPL";"CMPS";"CMPSCS";"CMPSEQ";"CMPSGE";"CMPSGT";"CMPSHI";"CMPSHS";

      "CMPSLE";"CMPSLO";"CMPSLS";"CMPSLT";"CMPSMI";"CMPSNE";"CMPSCC";"CMPSPL";"CMPSVC";"CMPSVS";"CMPVC";"CMPVS";

      "EOR";"EORCS";"EOREQ";"EORGE";"EORGT";"EORHI";"EORHS";"EORLE";"EORLO";"EORLS";"EORLT";"EORMI";"EORNE";"EORCC";

      "EORPL";"EORS";"EORSCS";"EORSEQ";"EORSGE";"EORSGT";"EORSHI";"EORSHS";"EORSLE";"EORSLO";"EORSLS";"EORSLT";

      "EORSMI";"EORSNE";"EORSCC";"EORSPL";"EORSVC";"EORSVS";"EORVC";"EORVS";"LSL";"LSLCS";"LSLEQ";"LSLGE";"LSLGT";

      "LSLHI";"LSLHS";"LSLLE";"LSLLO";"LSLLS";"LSLLT";"LSLMI";"LSLNE";"LSLCC";"LSLPL";"LSLS";"LSLSCS";"LSLSEQ";

      "LSLSGE";"LSLSGT";"LSLSHI";"LSLSHS";"LSLSLE";"LSLSLO";"LSLSLS";"LSLSLT";"LSLSMI";"LSLSNE";"LSLSCC";"LSLSPL";

      "LSLSVC";"LSLSVS";"LSLVC";"LSLVS";"LSR";"LSRCS";"LSREQ";"LSRGE";"LSRGT";"LSRHI";"LSRHS";"LSRLE";"LSRLO";

      "LSRLS";"LSRLT";"LSRMI";"LSRNE";"LSRCC";"LSRPL";"LSRS";"LSRSCS";"LSRSEQ";"LSRSGE";"LSRSGT";"LSRSHI";"LSRSHS";

      "LSRSLE";"LSRSLO";"LSRSLS";"LSRSLT";"LSRSMI";"LSRSNE";"LSRSCC";"LSRSPL";"LSRSVC";"LSRSVS";"LSRVC";"LSRVS";

      "MOV";"MOVCS";"MOVEQ";"MOVGE";"MOVGT";"MOVHI";"MOVHS";"MOVLE";"MOVLO";"MOVLS";"MOVLT";"MOVMI";"MOVNE";"MOVCC";

      "MOVPL";"MOVS";"MOVSCS";"MOVSEQ";"MOVSGE";"MOVSGT";"MOVSHI";"MOVSHS";"MOVSLE";"MOVSLO";"MOVSLS";"MOVSLT";

      "MOVSMI";"MOVSNE";"MOVSCC";"MOVSPL";"MOVSVC";"MOVSVS";"MOVVC";"MOVVS";"MVN";"MVNCS";"MVNEQ";"MVNGE";

      "MVNGT";"MVNHI";"MVNHS";"MVNLE";"MVNLO";"MVNLS";"MVNLT";"MVNMI";"MVNNE";"MVNCC";"MVNPL";"MVNS";"MVNSCS";

      "MVNSEQ";"MVNSGE";"MVNSGT";"MVNSHI";"MVNSHS";"MVNSLE";"MVNSLO";"MVNSLS";"MVNSLT";"MVNSMI";"MVNSNE";

      "MVNSCC";"MVNSPL";"MVNSVC";"MVNSVS";"MVNVC";"MVNVS";"ORR";"ORRCS";"ORREQ";"ORRGE";"ORRGT";"ORRHI";

      "ORRHS";"ORRLE";"ORRLO";"ORRLS";"ORRLT";"ORRMI";"ORRNE";"ORRCC";"ORRPL";"ORRS";"ORRSCS";"ORRSEQ";"ORRSGE";

      "ORRSGT";"ORRSHI";"ORRSHS";"ORRSLE";"ORRSLO";"ORRSLS";"ORRSLT";"ORRSMI";"ORRSNE";"ORRSCC";"ORRSPL";"ORRSVC";

      "ORRSVS";"ORRVC";"ORRVS";"ROR";"RORCS";"ROREQ";"RORGE";"RORGT";"RORHI";"RORHS";"RORLE";"RORLO";"RORLS";"RORLT";

      "RORMI";"RORNE";"RORCC";"RORPL";"RORS";"RORSCS";"RORSEQ";"RORSGE";"RORSGT";"RORSHI";"RORSHS";"RORSLE";"RORSLO";

      "RORSLS";"RORSLT";"RORSMI";"RORSNE";"RORSCC";"RORSPL";"RORSVC";"RORSVS";"RORVC";"RORVS";"RRX";"RRXCS";"RRXEQ";

      "RRXGE";"RRXGT";"RRXHI";"RRXHS";"RRXLE";"RRXLO";"RRXLS";"RRXLT";"RRXMI";"RRXNE";"RRXCC";"RRXPL";"RRXS";"RRXSCS";

      "RRXSEQ";"RRXSGE";"RRXSGT";"RRXSHI";"RRXSHS";"RRXSLE";"RRXSLO";"RRXSLS";"RRXSLT";"RRXSMI";"RRXSNE";"RRXSCC";

      "RRXSPL";"RRXSVC";"RRXSVS";"RRXVC";"RRXVS";"RSB";"RSBCS";"RSBEQ";"RSBGE";"RSBGT";"RSBHI";"RSBHS";"RSBLE";

      "RSBLO";"RSBLS";"RSBLT";"RSBMI";"RSBNE";"RSBCC";"RSBPL";"RSBS";"RSBSCS";"RSBSEQ";"RSBSGE";"RSBSGT";"RSBSHI";

      "RSBSHS";"RSBSLE";"RSBSLO";"RSBSLS";"RSBSLT";"RSBSMI";"RSBSNE";"RSBSCC";"RSBSPL";"RSBSVC";"RSBSVS";"RSBVC";

      "RSBVS";"RSC";"RSCCS";"RSCEQ";"RSCGE";"RSCGT";"RSCHI";"RSCHS";"RSCLE";"RSCLO";"RSCLS";"RSCLT";"RSCMI";"RSCNE";

      "RSCCC";"RSCPL";"RSCS";"RSCSCS";"RSCSEQ";"RSCSGE";"RSCSGT";"RSCSHI";"RSCSHS";"RSCSLE";"RSCSLO";"RSCSLS";"RSCSLT";

      "RSCSMI";"RSCSNE";"RSCSCC";"RSCSPL";"RSCSVC";"RSCSVS";"RSCVC";"RSCVS";"SBC";"SBCCS";"SBCEQ";"SBCGE";"SBCGT";

      "SBCHI";"SBCHS";"SBCLE";"SBCLO";"SBCLS";"SBCLT";"SBCMI";"SBCNE";"SBCCC";"SBCPL";"SBCS";"SBCSCS";"SBCSEQ";

      "SBCSGE";"SBCSGT";"SBCSHI";"SBCSHS";"SBCSLE";"SBCSLO";"SBCSLS";"SBCSLT";"SBCSMI";"SBCSNE";"SBCSCC";"SBCSPL";

      "SBCSVC";"SBCSVS";"SBCVC";"SBCVS";"SUB";"SUBCS";"SUBEQ";"SUBGE";"SUBGT";"SUBHI";"SUBHS";"SUBLE";"SUBLO";

      "SUBLS";"SUBLT";"SUBMI";"SUBNE";"SUBCC";"SUBPL";"SUBS";"SUBSCS";"SUBSEQ";"SUBSGE";"SUBSGT";"SUBSHI";"SUBSHS";

      "SUBSLE";"SUBSLO";"SUBSLS";"SUBSLT";"SUBSMI";"SUBSNE";"SUBSCC";"SUBSPL";"SUBSVC";"SUBSVS";"SUBVC";"SUBVS";

      "TEQ";"TEQCS";"TEQEQ";"TEQGE";"TEQGT";"TEQHI";"TEQHS";"TEQLE";"TEQLO";"TEQLS";"TEQLT";"TEQMI";"TEQNE";"TEQCC";

      "TEQPL";"TEQS";"TEQSCS";"TEQSEQ";"TEQSGE";"TEQSGT";"TEQSHI";"TEQSHS";"TEQSLE";"TEQSLO";"TEQSLS";"TEQSLT";

      "TEQSMI";"TEQSNE";"TEQSCC";"TEQSPL";"TEQSVC";"TEQSVS";"TEQVC";"TEQVS";"TST";"TSTCS";"TSTEQ";"TSTGE";"TSTGT";

      "TSTHI";"TSTHS";"TSTLE";"TSTLO";"TSTLS";"TSTLT";"TSTMI";"TSTNE";"TSTCC";"TSTPL";"TSTS";"TSTSCS";"TSTSEQ";

      "TSTSGE";"TSTSGT";"TSTSHI";"TSTSHS";"TSTSLE";"TSTSLO";"TSTSLS";"TSTSLT";"TSTSMI";"TSTSNE";"TSTSCC";"TSTSPL";

      "TSTSVC";"TSTSVS";"TSTVC";"TSTVS";"B";"BCS";"BEQ";"BGE";"BGT";"BHI";"BHS";"BL";"BLCS";"BLE";"BLEQ";"BLGE";

      "BLGT";"BLHI";"BLHS";"BLLE";"BLLO";"BLLS";"BLLT";"BLMI";"BLNE";"BLCC";"BLO";"BLPL";"BLS";"BLT";"BLVC";"BLVS";

      "BMI";"BNE";"BCC";"BPL";"BVC";"BVS";"END";"ENDCS";"ENDEQ";"ENDGE";"ENDGT";"ENDHI";"ENDHS";"ENDLE";"ENDLO";

      "ENDLS";"ENDLT";"ENDMI";"ENDNE";"ENDCC";"ENDPL";"ENDVC";"ENDVS";"LDM";"LDMCS";"LDMB";"LDMBCS";"LDMBEQ";"LDMBGE";

      "LDMBGT";"LDMBHI";"LDMBHS";"LDMBLE";"LDMBLO";"LDMBLS";"LDMBLT";"LDMBMI";"LDMBNE";"LDMBCC";"LDMBPL";"LDMBVC";

      "LDMBVS";"LDMDA";"LDMDACS";"LDMDAEQ";"LDMDAGE";"LDMDAGT";"LDMDAHI";"LDMDAHS";"LDMDALE";"LDMDALO";"LDMDALS";

      "LDMDALT";"LDMDAMI";"LDMDANE";"LDMDACC";"LDMDAPL";"LDMDAVC";"LDMDAVS";"LDMDB";"LDMDBCS";"LDMDBEQ";"LDMDBGE";

      "LDMDBGT";"LDMDBHI";"LDMDBHS";"LDMDBLE";"LDMDBLO";"LDMDBLS";"LDMDBLT";"LDMDBMI";"LDMDBNE";"LDMDBCC";"LDMDBPL";

      "LDMDBVC";"LDMDBVS";"LDMEA";"LDMEACS";"LDMEAEQ";"LDMEAGE";"LDMEAGT";"LDMEAHI";"LDMEAHS";"LDMEALE";"LDMEALO";

      "LDMEALS";"LDMEALT";"LDMEAMI";"LDMEANE";"LDMEACC";"LDMEAPL";"LDMEAVC";"LDMEAVS";"LDMED";"LDMEDCS";"LDMEDEQ";

      "LDMEDGE";"LDMEDGT";"LDMEDHI";"LDMEDHS";"LDMEDLE";"LDMEDLO";"LDMEDLS";"LDMEDLT";"LDMEDMI";"LDMEDNE";"LDMEDCC";

      "LDMEDPL";"LDMEDVC";"LDMEDVS";"LDMEQ";"LDMFA";"LDMFACS";"LDMFAEQ";"LDMFAGE";"LDMFAGT";"LDMFAHI";"LDMFAHS";

      "LDMFALE";"LDMFALO";"LDMFALS";"LDMFALT";"LDMFAMI";"LDMFANE";"LDMFACC";"LDMFAPL";"LDMFAVC";"LDMFAVS";"LDMFD";"LDMFDCS";

      "LDMFDEQ";"LDMFDGE";"LDMFDGT";"LDMFDHI";"LDMFDHS";"LDMFDLE";"LDMFDLO";"LDMFDLS";"LDMFDLT";"LDMFDMI";"LDMFDNE";

      "LDMFDCC";"LDMFDPL";"LDMFDVC";"LDMFDVS";"LDMGE";"LDMGT";"LDMHI";"LDMHS";"LDMIA";"LDMIACS";"LDMIAEQ";"LDMIAGE";

      "LDMIAGT";"LDMIAHI";"LDMIAHS";"LDMIALE";"LDMIALO";"LDMIALS";"LDMIALT";"LDMIAMI";"LDMIANE";"LDMIACC";"LDMIAPL";

      "LDMIAVC";"LDMIAVS";"LDMIB";"LDMIBCS";"LDMIBEQ";"LDMIBGE";"LDMIBGT";"LDMIBHI";"LDMIBHS";"LDMIBLE";"LDMIBLO";

      "LDMIBLS";"LDMIBLT";"LDMIBMI";"LDMIBNE";"LDMIBCC";"LDMIBPL";"LDMIBVC";"LDMIBVS";"LDMLE";"LDMLO";"LDMLS";"LDMLT";"LDMMI";

      "LDMNE";"LDMCC";"LDMPL";"LDMVC";"LDMVS";"LDR";"LDRCS";"LDRB";"LDRBCS";"LDRBEQ";"LDRBGE";"LDRBGT";"LDRBHI";

      "LDRBHS";"LDRBLE";"LDRBLO";"LDRBLS";"LDRBLT";"LDRBMI";"LDRBNE";"LDRBCC";"LDRBPL";"LDRBVC";"LDRBVS";"LDRDA";

      "LDRDACS";"LDRDAEQ";"LDRDAGE";"LDRDAGT";"LDRDAHI";"LDRDAHS";"LDRDALE";"LDRDALO";"LDRDALS";"LDRDALT";"LDRDAMI";

      "LDRDANE";"LDRDACC";"LDRDAPL";"LDRDAVC";"LDRDAVS";"LDRDB";"LDRDBCS";"LDRDBEQ";"LDRDBGE";"LDRDBGT";"LDRDBHI";

      "LDRDBHS";"LDRDBLE";"LDRDBLO";"LDRDBLS";"LDRDBLT";"LDRDBMI";"LDRDBNE";"LDRDBCC";"LDRDBPL";"LDRDBVC";"LDRDBVS";

      "LDREA";"LDREACS";"LDREAEQ";"LDREAGE";"LDREAGT";"LDREAHI";"LDREAHS";"LDREALE";"LDREALO";"LDREALS";"LDREALT";

      "LDREAMI";"LDREANE";"LDREACC";"LDREAPL";"LDREAVC";"LDREAVS";"LDRED";"LDREDCS";"LDREDEQ";"LDREDGE";"LDREDGT";"LDREDHI";

      "LDREDHS";"LDREDLE";"LDREDLO";"LDREDLS";"LDREDLT";"LDREDMI";"LDREDNE";"LDREDCC";"LDREDPL";"LDREDVC";"LDREDVS";"LDREQ";

      "LDRFA";"LDRFACS";"LDRFAEQ";"LDRFAGE";"LDRFAGT";"LDRFAHI";"LDRFAHS";"LDRFALE";"LDRFALO";"LDRFALS";"LDRFALT";"LDRFAMI";

      "LDRFANE";"LDRFACC";"LDRFAPL";"LDRFAVC";"LDRFAVS";"LDRFD";"LDRFDCS";"LDRFDEQ";"LDRFDGE";"LDRFDGT";"LDRFDHI";"LDRFDHS";

      "LDRFDLE";"LDRFDLO";"LDRFDLS";"LDRFDLT";"LDRFDMI";"LDRFDNE";"LDRFDCC";"LDRFDPL";"LDRFDVC";"LDRFDVS";"LDRGE";"LDRGT";

      "LDRHI";"LDRHS";"LDRIA";"LDRIACS";"LDRIAEQ";"LDRIAGE";"LDRIAGT";"LDRIAHI";"LDRIAHS";"LDRIALE";"LDRIALO";"LDRIALS";

      "LDRIALT";"LDRIAMI";"LDRIANE";"LDRIACC";"LDRIAPL";"LDRIAVC";"LDRIAVS";"LDRIB";"LDRIBCS";"LDRIBEQ";"LDRIBGE";"LDRIBGT";

      "LDRIBHI";"LDRIBHS";"LDRIBLE";"LDRIBLO";"LDRIBLS";"LDRIBLT";"LDRIBMI";"LDRIBNE";"LDRIBCC";"LDRIBPL";"LDRIBVC";"LDRIBVS";

      "LDRLE";"LDRLO";"LDRLS";"LDRLT";"LDRMI";"LDRNE";"LDRCC";"LDRPL";"LDRVC";"LDRVS";"STM";"STMCS";"STMB";"STMBCS";"STMBEQ";

      "STMBGE";"STMBGT";"STMBHI";"STMBHS";"STMBLE";"STMBLO";"STMBLS";"STMBLT";"STMBMI";"STMBNE";"STMBCC";"STMBPL";"STMBVC";

      "STMBVS";"STMDA";"STMDACS";"STMDAEQ";"STMDAGE";"STMDAGT";"STMDAHI";"STMDAHS";"STMDALE";"STMDALO";"STMDALS";"STMDALT";

      "STMDAMI";"STMDANE";"STMDACC";"STMDAPL";"STMDAVC";"STMDAVS";"STMDB";"STMDBCS";"STMDBEQ";"STMDBGE";"STMDBGT";"STMDBHI";

      "STMDBHS";"STMDBLE";"STMDBLO";"STMDBLS";"STMDBLT";"STMDBMI";"STMDBNE";"STMDBCC";"STMDBPL";"STMDBVC";"STMDBVS";"STMEA";

      "STMEACS";"STMEAEQ";"STMEAGE";"STMEAGT";"STMEAHI";"STMEAHS";"STMEALE";"STMEALO";"STMEALS";"STMEALT";"STMEAMI";"STMEANE";

      "STMEACC";"STMEAPL";"STMEAVC";"STMEAVS";"STMED";"STMEDCS";"STMEDEQ";"STMEDGE";"STMEDGT";"STMEDHI";"STMEDHS";"STMEDLE";

      "STMEDLO";"STMEDLS";"STMEDLT";"STMEDMI";"STMEDNE";"STMEDCC";"STMEDPL";"STMEDVC";"STMEDVS";"STMEQ";"STMFA";"STMFACS";

      "STMFAEQ";"STMFAGE";"STMFAGT";"STMFAHI";"STMFAHS";"STMFALE";"STMFALO";"STMFALS";"STMFALT";"STMFAMI";"STMFANE";"STMFACC";

      "STMFAPL";"STMFAVC";"STMFAVS";"STMFD";"STMFDCS";"STMFDEQ";"STMFDGE";"STMFDGT";"STMFDHI";"STMFDHS";"STMFDLE";"STMFDLO";

      "STMFDLS";"STMFDLT";"STMFDMI";"STMFDNE";"STMFDCC";"STMFDPL";"STMFDVC";"STMFDVS";"STMGE";"STMGT";"STMHI";"STMHS";"STMIA";

      "STMIACS";"STMIAEQ";"STMIAGE";"STMIAGT";"STMIAHI";"STMIAHS";"STMIALE";"STMIALO";"STMIALS";"STMIALT";"STMIAMI";"STMIANE";

      "STMIACC";"STMIAPL";"STMIAVC";"STMIAVS";"STMIB";"STMIBCS";"STMIBEQ";"STMIBGE";"STMIBGT";"STMIBHI";"STMIBHS";"STMIBLE";

      "STMIBLO";"STMIBLS";"STMIBLT";"STMIBMI";"STMIBNE";"STMIBCC";"STMIBPL";"STMIBVC";"STMIBVS";"STMLE";"STMLO";"STMLS";"STMLT";

      "STMMI";"STMNE";"STMCC";"STMPL";"STMVC";"STMVS";"STR";"STRCS";"STRB";"STRBCS";"STRBEQ";"STRBGE";"STRBGT";"STRBHI";"STRBHS";

      "STRBLE";"STRBLO";"STRBLS";"STRBLT";"STRBMI";"STRBNE";"STRBCC";"STRBPL";"STRBVC";"STRBVS";"STRDA";"STRDACS";"STRDAEQ";

      "STRDAGE";"STRDAGT";"STRDAHI";"STRDAHS";"STRDALE";"STRDALO";"STRDALS";"STRDALT";"STRDAMI";"STRDANE";"STRDACC";"STRDAPL";

      "STRDAVC";"STRDAVS";"STRDB";"STRDBCS";"STRDBEQ";"STRDBGE";"STRDBGT";"STRDBHI";"STRDBHS";"STRDBLE";"STRDBLO";"STRDBLS";

      "STRDBLT";"STRDBMI";"STRDBNE";"STRDBCC";"STRDBPL";"STRDBVC";"STRDBVS";"STREA";"STREACS";"STREAEQ";"STREAGE";"STREAGT";"STREAHI";

      "STREAHS";"STREALE";"STREALO";"STREALS";"STREALT";"STREAMI";"STREANE";"STREACC";"STREAPL";"STREAVC";"STREAVS";"STRED";"STREDCS";

      "STREDEQ";"STREDGE";"STREDGT";"STREDHI";"STREDHS";"STREDLE";"STREDLO";"STREDLS";"STREDLT";"STREDMI";"STREDNE";"STREDCC";"STREDPL";

      "STREDVC";"STREDVS";"STREQ";"STRFA";"STRFACS";"STRFAEQ";"STRFAGE";"STRFAGT";"STRFAHI";"STRFAHS";"STRFALE";"STRFALO";"STRFALS";

      "STRFALT";"STRFAMI";"STRFANE";"STRFACC";"STRFAPL";"STRFAVC";"STRFAVS";"STRFD";"STRFDCS";"STRFDEQ";"STRFDGE";"STRFDGT";"STRFDHI";

      "STRFDHS";"STRFDLE";"STRFDLO";"STRFDLS";"STRFDLT";"STRFDMI";"STRFDNE";"STRFDCC";"STRFDPL";"STRFDVC";"STRFDVS";"STRGE";"STRGT";

      "STRHI";"STRHS";"STRIA";"STRIACS";"STRIAEQ";"STRIAGE";"STRIAGT";"STRIAHI";"STRIAHS";"STRIALE";"STRIALO";"STRIALS";"STRIALT";

      "STRIAMI";"STRIANE";"STRIACC";"STRIAPL";"STRIAVC";"STRIAVS";"STRIB";"STRIBCS";"STRIBEQ";"STRIBGE";"STRIBGT";"STRIBHI";"STRIBHS";

      "STRIBLE";"STRIBLO";"STRIBLS";"STRIBLT";"STRIBMI";"STRIBNE";"STRIBCC";"STRIBPL";"STRIBVC";"STRIBVS";"STRLE";"STRLO";"STRLS";

      "STRLT";"STRMI";"STRNE";"STRCC";"STRPL";"STRVC";"STRVS";"EQU";"DCD";"DCB";"FILL";"SPACE";"ADR";"ADREQ";"ADRNE";"ADRMI";"ADRPL";

      "ADRHI";"ADRHS";"ADRLO";"ADRLS";"ADRGE";"ADRGT";"ADRLE";"ADRLT";"ADRVS";"ADRVC";"ADRCC";"ADRCS";
      |]
      |> Array.toList

let token =
    "arm",
     createObj [
        "defaultToken" ==> "invalid"
        "ignoreCase" ==> true
        "brackets" ==> [
            ["{" ; "}" ; "delimiter.curly" ]
            ["[" ; "]" ; "delimiter.square" ]
            ["(" ; ")" ; "delimiter.parenthesis" ]
            ["<" ; ">" ; "delimiter.angle" ]
        ]

        "operators" ==> [
          "+" ; "-" ; "*" ; "," ; "="
        ]

        "keywords" ==> keywordsLst

    ]


//// we include these common regular expressions
  //  symbols: /[=!~?:&|+\-*\/\^%]+/,

  //  // C# style strings
  //  escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

let tokeniZer = createObj [
    //root ==> [
        //// identifiers and keywords
        //[ /[a-z_$][\w$]*/, createObj [
        //    cases ==> createObj [
        //        "@keywords" ==> "keyword"
        //        "@default" ==> "identifier"
        //        ]
        //    ]
        //]

//      // whitespace
//      { include: '@whitespace' },

//      // delimiters and operators
//      [/[{}()\[\]]/, '@brackets'],
//      //[/[<>](?!@symbols)/, '@brackets'],
//      [/@symbols/, {
//        cases: {
//          '@operators': 'symbol.operator',
//          '@default': 'symbol.other'
//        }
//      }],


//      // numbers
    
//      [/#-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.hash.hex'],
//      [/#-?0[bB][0-1][01_]*/, 'number.hash.bin'],
//      [/#-?\d[\d_]*/, 'number.hash'],
//      [/-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.bare.hex'],
//      [/-?0[bB][0-1][01_]*/, 'number.bare.bin'],
//      [/-?\d[\d_]*/, 'number.bare'],

//      // delimiter: after number because of .\d floats
//      [/[,.]/, 'delimiter'],

//      // strings
//      [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
//      [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

//      // characters
//      [/'[^\\']'/, 'string'],
//      [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
//      [/'/, 'string.invalid'],

//      // ARM comments
//      [/;(.*)/, 'comment'],
//      [/>>;(.*)/, 'comment.testpass'],
//      [/>>-(.*)/, 'comment.testerror'],

//    ],


    //string ==> [
    //  [/[^\\"]+/ 'string'],
    //  [/@escapes/, 'string.escape'],
    //  [/\\./, 'string.escape.invalid'],
    //  [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
    //],

//    whitespace: [
//      [/[ \t\r\n]+/, 'white'],
//      //        [/\/\*/, 'comment', '@comment'],
//      //        [/\/\/.*$/, 'comment'],
//    ],
//  }
//}
    ]
