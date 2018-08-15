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

open CommonData
open Memory

let editorOptions (readOnly:bool) = 
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
    let eo = editorOptions readOnly
    Refs.editors.[tId]?updateOptions(eo) |> ignore

let setTheme theme = 
    window?monaco?editor?setTheme(theme)


let updateAllEditors readOnly =
    Refs.editors
    |> Map.iter (fun tId _ -> if tId = Refs.currentFileTabId then  readOnly else false
                              |> updateEditor tId)
    let theme = Refs.vSettings.EditorTheme
    Refs.setFilePaneBackground (
        match theme with 
        | "one-light-pro" | "solarised-light" -> "white" 
        | _ -> "black")
    setTheme (theme) |> ignore
    setCustomCSS "--editor-font-size" (sprintf "%spx" vSettings.EditorFontSize)
   

// Disable the editor and tab selection during execution
let disableEditors () = 
    Refs.fileTabMenu.classList.add("disabled-click")
    Refs.fileTabMenu.onclick <- (fun _ ->
        Browser.window.alert("Cannot change tabs during execution")
    )
    updateEditor Refs.currentFileTabId true
    Refs.darkenOverlay.classList.remove("invisible")
    Refs.darkenOverlay.classList.add([|"disabled-click"|])

// Enable the editor once execution has completed
let enableEditors () =
    Refs.fileTabMenu.classList.remove("disabled-click")
    Refs.fileTabMenu.onclick <- ignore
    updateEditor Refs.currentFileTabId false
    Refs.darkenOverlay.classList.add([|"invisible"|])

let mutable decorations : obj list = []
let mutable lineDecorations : obj list = []

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
let removeEditorDecorations tId =
    List.iter (fun x -> removeDecorations Refs.editors.[tId] x) decorations
    decorations <- []

let editorLineDecorate editor number decoration (rangeOpt : ((int*int) option)) =
    let model = editor?getModel()
    let lineWidth = model?getLineMaxColumn(number)
    let posStart = match rangeOpt with | None -> 1 | Some (n,_) -> n
    let posEnd = match rangeOpt with | None -> lineWidth :?> int | Some (_,n) -> n
    let newDecs = lineDecoration editor
                    decorations
                    (monacoRange number posStart number posEnd)
                    decoration
    decorations <- List.append decorations [newDecs]

// highlight a particular line
let highlightLine tId number className = 
    editorLineDecorate 
        Refs.editors.[tId]
        number
        (createObj[
            "isWholeLine" ==> true
            "inlineClassName" ==> className
        ])
        None

let highlightGlyph tId number glyphClassName = 
    editorLineDecorate 
        Refs.editors.[tId]
        number
        (createObj[
            "isWholeLine" ==> true
            "glyphMarginClassName" ==> glyphClassName
        ])
        None

let highlightNextInstruction tId number =
    highlightGlyph tId number "editor-glyph-margin-arrow"

/// Decorate a line with an error indication and set up a hover message
/// Distinct message lines must be elements of markdownLst
/// markdownLst: string list - list of markdown paragraphs
/// tId: int - tab identifier
/// lineNumber: int - line to decorate, starting at 1
let makeErrorInEditor tId lineNumber (hoverLst:string list) (gHoverLst: string list) = 
    let makeMarkDown textLst =
        textLst
        |> List.toArray
        |> Array.map (fun txt ->  createObj [ "isTrusted" ==> true; "value" ==> txt ])
    // decorate the line
    editorLineDecorate 
        Refs.editors.[tId]
        lineNumber 
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            "inlineClassName" ==> "editor-line-error"
            "hoverMessage" ==> makeMarkDown hoverLst
            //"glyphMarginClassName" ==> "editor-glyph-margin-error"
            //"glyphMarginHoverMessage" ==> makeMarkDown gHoverLst
        ])
        None
    // decorate the margin
    editorLineDecorate 
        Refs.editors.[tId]
        lineNumber 
        (createObj [
            "isWholeLine" ==> true
            "isTrusted" ==> true
            //"inlineClassName" ==> "editor-line-error"
            //"hoverMessage" ==> makeMarkDown hoverLst
            //"inlineClassName" ==> "editor-line-error"
            "glyphMarginClassName" ==> "editor-glyph-margin-error"
            "glyphMarginHoverMessage" ==> makeMarkDown gHoverLst
            "overviewRuler" ==> createObj [ "position" ==> 4 ]
        ])
        None

let revealLineInWindow tId (lineNumber: int) =
    Refs.editors.[tId]?revealLineInCenterIfOutsideViewport(lineNumber) |> ignore

//*************************************************************************************
//                              EDITOR CONTENT WIDGETS
//*************************************************************************************

type MemDirection = MemRead | MemWrite

type WidgetPlace =
    | AboveBelow of HPos: int * VPos: int
    | Exact of HPos: int * VPos: int


let makeContentWidget (name: string) (dom:HTMLElement) (pos:WidgetPlace) =
    let h,v = match pos with | AboveBelow(h,v) -> (h,v) | Exact(h,v) -> (h,v)
    let widget = createObj  [
                  "domNode" ==> dom
                  "getDomNode" ==> fun () -> dom
                  "getId" ==> fun () -> name
                  "getPosition" ==> 
                     fun () -> createObj [
                                "position" ==>  createObj [
                                    "lineNumber" ==> v
                                    "column" ==> h
                                    ]
                                "preference" ==>
                                    match pos with 
                                    | Exact _ -> [|0|]
                                    | AboveBelow _ -> [|1;2|]
                                ]
             ] 
    Refs.editors.[Refs.currentFileTabId]?addContentWidget widget |> ignore
    Refs.currentTabWidgets <- Map.add name widget Refs.currentTabWidgets 

let deleteContentWidget name =
    match Map.tryFind name Refs.currentTabWidgets with
    | None -> ()
    | Some w ->
        Refs.editors.[Refs.currentFileTabId]?removeContentWidget w |> ignore
        Refs.currentTabWidgets <- Map.remove name Refs.currentTabWidgets

let deleteAllContentWidgets() =
    Array.iter deleteContentWidget (Map.keys Refs.currentTabWidgets) 
            

let makeEditorInfoButton h v (buttonText:string) (toolTipDOM:HTMLElement) = 
    let name = buttonText.ToLower()
    let domID = sprintf "info-button-%s-%d" name v
    let tooltip = Refs.ELEMENT "DIV" [sprintf "tooltip-%s" name] [toolTipDOM]
    let dom = 
        Refs.ELEMENT "BUTTON" [ sprintf "info-button-%s" name] [] 
        |> Refs.INNERHTML buttonText 
        |> Refs.ID domID
        |> Refs.STYLE ("margin-left",sprintf "%.0fpx" (0.6 * (float h+2.0) * float (int vSettings.EditorFontSize)))
    dom.addEventListener_click( fun _ ->
        Browser.console.log (sprintf "Clicking button %s" buttonText) |> ignore
        )
    deleteContentWidget domID // in some cases we may be updating an existing widget
    makeContentWidget domID dom <| Exact(0,v)
    Refs.tippy( "#"+domID, createObj <| 
        [ 
            "html" ==> tooltip 
            "hideOnClick" ==> "persistent"
            "interactive" ==> true
            "arrow" ==> true
            "arrowType"==> "round"
            "theme" ==> 
                match Refs.vSettings.EditorTheme with
                | "one-light-pro" | "solarised-light" -> "dark"
                | _ -> "light"
        ])


let memStackInfo (ins: Memory.InstrMemMult) (dir: MemDirection) (dp: DataPath) =
    failwithf "Not implemented"

let findCodeEnd  (lineCol:int) =
    let tabSize = 6
    match Refs.currentTabText() with
    | None -> 0
    | Some text ->
        if text.Length <= lineCol then
            0
        else
            let line = text.[lineCol]
            match String.splitRemoveEmptyEntries [|';'|] line |> Array.toList with
            | s :: _ -> (s.Length / tabSize)*tabSize + (if s.Length % tabSize > 0 then tabSize else 0)
            | [] -> 0

let toolTipInfo (v: int) (dp: DataPath) ((cond,instruction): ParseTop.CondInstr) =
    match Helpers.condExecute cond dp, instruction with
    | false,_ -> ()
    | true, ParseTop.IMEM ins -> 
        match Memory.executeMem ins dp with
        | Error _ -> ()
        | Ok res -> 
            let TROWS = List.map (fun s -> s |> toDOM |> TD) >> TROW
            let memPointerInfo (ins: Memory.InstrMemSingle) (dir: MemDirection) (dp: DataPath) =
                let baseAddrU = dp.Regs.[ins.Rb]
                let baseAddr = int32 baseAddrU
                let offset = (ins.MAddr dp baseAddr |> uint32) - baseAddrU
                let ea = match ins.MemMode with | Memory.PreIndex | Memory.NoIndex -> (baseAddrU + offset) | _ -> baseAddrU
                let mData = (match ins.MemSize with | MWord -> Memory.getDataMemWord | MByte -> Memory.getDataMemByte) ea dp
                (findCodeEnd v, "Pointer"), TABLE [] [
                    TROWS [sprintf "Base (%s)" (ins.Rb.ToString()) ; sprintf "0x%08X" baseAddrU]
                    TROWS ["Address";  ea |> sprintf "0x%08X"]
                    TROWS ["Offset";  offset |> sprintf "0x%08X"]
                    TROWS ["Increment"; match ins.MemMode with | Memory.NoIndex ->  0u | _ -> offset
                                            |> sprintf "%d"]
                    TROWS ["Data"; match ins.LSType with 
                                   | LOAD -> match mData with | Ok dat -> dat | _ -> 0u
                                   | STORE -> dp.Regs.[ins.Rd] 
                                   |> fun d ->
                                        match ins.MemSize with
                                        | MWord -> sprintf "0x%08X" d
                                        | MByte -> sprintf "0x%02X" ((uint32 d) % 256u)]
                    ]
        
            let makeTip memInfo =
                let (hOffset, label), tipDom = memInfo dp
                makeEditorInfoButton hOffset (v+1) label tipDom
            match ins with
            | Memory.LDR ins -> makeTip <| memPointerInfo ins MemRead
            | Memory.STR ins -> makeTip <| memPointerInfo ins MemWrite
            | Memory.LDM ins -> makeTip <| memStackInfo  ins MemRead
            | Memory.STM ins -> makeTip <| memStackInfo ins MemWrite
            | _ -> ()
    | _ -> ()

let tooltipInfo (dp : DataPath) (code:CodeMemory<ParseTop.CondInstr*int>) =
    failwithf "Not implemented"   