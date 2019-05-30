(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tooltips
    Description: Code to implement tooltips and dynamic graphical popups
*)

/// F# References to static parts of renderer DOM
module Tooltips

open Elmish
open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
open Microsoft.FSharp.Collections
open EEExtensions
open Fable.Helpers.React
open Fable.Core
open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
open Microsoft.FSharp.Collections
open Fable.Helpers.React
open Fable.Helpers.React.Props
open System.Diagnostics
open System
open Refs
open CommonData
open Monaco

//------------------------------------------TIPPY.JS----------------------------------------------

/// TIPPY.JS is for the 
/// top-level function from tippy.js to make tooltips
let tippy (rClass : string, tippyOpts : obj) : unit = importDefault "tippy.js"

let deleteContentWidget currentTabWidgets editor name =
    match Map.tryFind name currentTabWidgets with
    | None -> ()
    | Some w -> editor?removeContentWidget w |> ignore

/// delete all content widgets
let deleteAllContentWidgets currentTabWidgets editor =
    Array.iter (deleteContentWidget currentTabWidgets editor) (Map.keys currentTabWidgets)

//------------------------------------------REACT-TIPPY----------------------------------------------
    
let inline tooltips (props: TooltipsProps list) 
                    (elLst: React.ReactElement list) : React.ReactElement =
    ofImport "default" 
             "@tippy.js/react" 
             (keyValueList CaseRules.LowerFirst props) 
             elLst

let basicTooltipsPropsLst : TooltipsProps list =
    [ Animation "fade"
      Arrow true
      Animation "fade"
      Refs.Theme "bootstrap"
      Distance 7
      HideOnClick false ]   

let defaultTooltipsPropsLst =
    [
        Delay (1000, 0)
        Placement "bottom"
    ] @ 
    basicTooltipsPropsLst

// ***********************************************************************************
//                            strings for the tooltips
// ***********************************************************************************

let repTooltipStr =
    function
    | Hex -> "Switch numeric displays to hexadecimal"
    | Bin -> "Switch numeric displays to binary. 
              In binary '_' is separator used to make bits more readable optional in assembler literals"
    | Dec -> "Switch numeric displays to two's complement signed decimal"
    | UDec -> "Switch numeric displays to unsigned decimal"

let viewTooltipStr =
    function
    | Registers -> "Displays current register contents"
    | Memory -> "Displays current data memory contents after execution has started. 
                 Words are added dynamically when they are written"
    | Symbols -> "Displays symbols (labels) after execution has started"

let regTooltipStr =
    function
    | R13 -> "R13 (SP) is the Stack Pointer. 
              It can be used as a data register. 
              SP is initialised to a value in high memory at the start of simulation by Visual2 to facilitate  use of stacks"
    | R14 -> "R14 (LR) is the Link Register. 
              It can be used as a data register"
    | R15 -> "R15 (PC) is the Program Counter. 
              It cannot be used as a data register"
    | x -> (string x) + " is a data register"

let flagTooltipStr = 
    "ARM Status bits (Flags) NZCV. 
     Blue indicates that Flag was written by the most recently executed instruction."

let clockSymTooltipStr = "Execution time"

let clockTooltipStr = "Instructions : clock cycles"

/// work out tippy theme with opposite background to editor
let tippyTheme =
    function
    | "one-light-pro" | "solarised-light" -> "dark"
    | _ -> "light"

/// <summary> Make an editor content widget to display over editor.
/// Position in editor: pos. HTML to display: dom.</summary>
/// <param name="name"> Widget id (tracked by editor) name</param>
/// <param name="dom"> HTML to display in widget </param>
/// <param name="pos">  Position of widget on editor character grid </param>
let makeContentWidget (name : string) (dom : HTMLElement) (editor : Monaco.Editor.IEditor option) currentTabWidgets (pos : WidgetPlace)  =
    let h, v = match pos with | AboveBelow(h, v) -> (h, v) | Exact(h, v) -> (h, v)
    let widget = createObj [
                  "domNode" ==> dom
                  "getDomNode" ==> fun () -> dom
                  "getId" ==> fun () -> name
                  "getPosition" ==>
                       fun () ->
                        createObj [ "position" ==>
                                        createObj [
                                             "lineNumber" ==> v
                                             "column" ==> h
                                        ]
                                    "preference" ==>
                                        match pos with
                                        | Exact _ -> [| 0 |]
                                        | AboveBelow _ -> [| 1; 2 |]
                                  ]
                  ]
    editor?addContentWidget widget |> ignore
    Map.add name widget currentTabWidgets

/// <summary> Tooltip generator using tippy.js https://atomiks.github.io/tippyjs/ </summary>
/// <param name="theme"> Tippy theme as defined in CSS: dark, light, etc</param>
/// <param name="placement"> Tippy placement (top, bottom, etc) </param>
/// <param name="clickable"> true => click to display, false => hover to display </param>
/// <param name="button"> true => delay tooltip and make it close on click </param>
/// <param name="domID"> ID of DOM element to which tooltip is attached </param>
let makeTooltip (theme : string) (placement : string) (clickable : bool) (button : bool) (domID : string) (tooltip : HTMLElement) =
    tippy ("#" + domID, createObj <|
        [
            "html" ==> tooltip
            "hideOnClick" ==> if clickable then true :> obj else button :> obj
            "interactive" ==> not clickable
            "delay" ==> if button then "[1300,0]" else "0"
            "arrow" ==> true
            "trigger" ==> if clickable then "click" else "mouseenter"
            "arrowType" ==> "round"
            "theme" ==> theme
            "placement" ==> placement
        ])

/// Draw text with given alignment on SVG
let svgText alignX alignY txtClass posX posY txt =
    let fS f = sprintf "%.2f" f
    text [
        X(posX |> fS);
        Y(posY |> fS);
        !!("dominantBaseline", alignY);
        SVGAttr.TextAnchor alignX
        !!("className", txtClass)
    ] [ ofString txt ]

/// Draw text with LHS middle alignment
let labelText = svgText "end" "middle"

/// Draw a box with text inside
let textInBox (width, height) (boxClass : string) (txtClass : string) (rhTopX, rhTopY) txt =
    let fS f = sprintf "%.2f" f
    svgEl "g" [] [
        rect [
            X(fS rhTopX)
            Y(fS rhTopY)
            SVGAttr.Height(fS height)
            SVGAttr.Width(fS width)
            !!("dominantBaseline", "middle") //align vertically on centre
            SVGAttr.TextAnchor "middle" // align horizontally on centre
            !!("className", boxClass)
        ] []
        text [
            X(rhTopX + width / 2.0 |> fS);
            Y(rhTopY + height / 2.0 |> fS);
            !!("dominantBaseline", "middle");
            SVGAttr.TextAnchor "middle"
            !!("className", txtClass)
        ] [ ofString txt ]
    ]

/// Draw arrow from (x1,y1) to (x2,y2) of given color and width
let arrowV width color (x1, y1) (x2, y2) =
    let head = width * 5.
    let al = sqrt ((x1 - x2) ** 2. + (y1 - y2) ** 2.)
    let headX = head * (x2 - x1) / al
    let headY = head * (y2 - y1) / al
    let fS f = sprintf "%.2f" f
    line [
            X1(fS x1);
            Y1(fS y1);
            X2(fS (x2 - headX));
            Y2(fS (y2 - headY));
            SVGAttr.StrokeWidth(fS width);
            SVGAttr.Stroke color;
            SVGAttr.MarkerEnd(sprintf "url(#arrowHead-%s)" color)
         ] []

/// draw a normal size arrow
let arrow color (x1, y1) (x2, y2) = arrowV 0.2 color (x1, y1) (x2, y2)


/// Draw a set of 32 horizontally aligned boxes with bits inside as a register
let register boxClass txtClass (boxW, boxH) (posX, posY) (bits : int list) =
    let box xp yp b =
        let txt = sprintf "%d" b
        textInBox (boxW, boxH) boxClass txtClass (xp, yp) txt
    let boxes =
        bits
        |> List.indexed
        |> List.rev
        |> List.map (fun (n, b) ->
            let xp = (float n) * boxW + posX
            box xp posY b)
    svgEl "g" [] boxes

/// define SVG marker for arrowhead. mId = marker ID. color = marker color.
let arrowMarker mId color =
    defs [] [
                svgEl "marker" [
                    !!("id", mId);
                    !!("markerWidth", "5");
                    !!("markerHeight", "5");
                    !!("refX", "0");
                    !!("refY", "2.5");
                    !!("orient", "auto");
                    !!("markerUnits", "strokeWidth")
                    SVGAttr.Stroke color
                ] [ path [ D "M0,1 L0,4 L4.5,2.5 z"; SVGAttr.Fill color ] [] ]
            ]

/// Include all markers used here for SVG diagrams
/// This function must be inserted in SVG just once before other descriptions.
let svgMarkerDefs() =
    svgEl "g" [] [
        arrowMarker "arrowHead-black" "black"
        arrowMarker "arrowHead-red" "red"
        ]

let textLines textOut xPos yPos lineV lines =
    svgEl "g" [] (
        lines
        |> List.indexed
        |> List.map (fun (i, txt) -> yPos + (float i) * lineV, txt)
        |> List.map (fun (yP, txt) -> textOut xPos yP txt)
        )

/// Draw text with middle bottom alignment
let colText = svgText "middle" "bottom"

let makeHtmlFromSVG re =
    let ele = ELEMENT "div" [] []
    ReactDom.render (re, ele)
    ele

/// Generate an SVG diagram for shifts as HTML DOM
let displayShiftDiagram rn (beforeNum, beforeC) (op2Num, (rDest, destNum), op2C, finalC, writeC, alu) (shiftT : DP.ArmShiftType option) shiftNum =
    let boxW, boxH = 2.7, 2.7
    let posX, posY = 30., 3.
    let posLabX = 29.
    let aluW, aluH = 20., 10.
    let posAluX = posX + boxW * 16. - aluW / 2.
    let posAluY = posY + 35.
    let sepY = 25.
    let sepY' = posAluY + aluH + 7. - posY
    let carryNX = 8
    let posCX = posX - (float carryNX) * boxW
    let boxClass = "tooltip-shift-reg-box"
    let carryBoxClass = "tooltip-shift-carry-box"
    let aluTxtClass = "tooltip-shift-alu-txt"
    let txtClass = "tooltip-shift-reg-txt"
    let makeLabel = labelText txtClass posLabX
    let svgIfTrue b el =
        svgEl "g" [] (if b then el else [])


    let carryBox yp b =
        let txt = sprintf "%d" b
        textInBox (boxW, boxH) carryBoxClass txtClass (posCX, yp) txt

    let arrow' color startN endN = arrow color (boxW / 2. + posX + (float startN) * boxW, posY + boxH) (boxW / 2. + posX + (float endN) * boxW, posY + sepY)

    let arrowSet startN endN num =
        svgEl "g" [] (List.map (fun i -> arrow' "black" (startN + i) (endN + i)) [ 0..num - 1 ])

    let getBits num =
        [ 31..-1..0 ]
        |> List.map (fun n -> match (num &&& (1 <<< n)) with | 0 -> 0 | _ -> 1)

    let reg = register boxClass txtClass (boxW, boxH)

    let arrows =
            match shiftT with
            | Some DP.LSR
            | Some DP.ASR ->
                [
                    svgIfTrue writeC [ arrow' "red" (32 - shiftNum) (-carryNX) ]
                    arrowSet 0 shiftNum (32 - shiftNum)
                    svgIfTrue (shiftT = Some DP.ASR) <| List.map (fun n -> arrow' "black" 0 n) [ 0..shiftNum - 1 ]
                ]
            | Some DP.LSL ->
                [
                    svgIfTrue writeC [ arrow' "red" (shiftNum - 1) (-carryNX) ]
                    arrowSet shiftNum 0 (32 - shiftNum)
                ]
            | Some DP.ROR ->
                [
                    svgIfTrue writeC [ arrow' "red" (32 - shiftNum) (-carryNX) ]
                    arrowSet 0 shiftNum (32 - shiftNum)
                    arrowSet (32 - shiftNum) 0 shiftNum
                ]

            | None -> // RRX
                [
                    svgIfTrue writeC [ arrow' "red" 31 (-carryNX) ]
                    arrow' "red" (-carryNX) 0
                    arrowSet 0 1 31

                ]

    svg
        [ ViewBox "0 0 120 59"; unbox ("width", "700px") ] (
        [
            svgMarkerDefs() // used to define arrow heads
            carryBox posY beforeC
            carryBox (posY + sepY) (if writeC then op2C else beforeC)
            textLines (labelText txtClass) (posCX - 2.) (posY - 2. + sepY + boxH / 2.) 2. [ "Shift"; "bit"; "out" ]
            carryBox (posY + sepY') finalC
            svgIfTrue (not writeC) [ arrow' "red" -carryNX -carryNX ]
            svgIfTrue (not (alu && writeC)) [ arrow "red" (boxW / 2. + posCX, posY + sepY + boxH) (boxW / 2. + posCX, posY + sepY') ]
            svgIfTrue (alu && writeC) [ arrow "red" (posAluX, posAluY + aluH / 2.) (posCX + boxW, posY + sepY' + boxH / 2.) ]
            textInBox (aluW, aluH) boxClass aluTxtClass (posAluX, posAluY) "ALU"
            svgText "start" "middle" txtClass (posAluX + 0.3) (posAluY + aluH / 2.) "Cout"
            arrowV 0.5 "black" (posAluX + aluW / 2., posAluY + aluH) (posAluX + aluW / 2., posAluY + aluH + 6.)
            arrowV 0.5 "black" (posAluX + aluW / 2., posAluY - 6.) (posAluX + aluW / 2., posAluY)
            arrowV 0.5 "black" (posAluX + aluW + 6., posAluY + aluH / 2.) (posAluX + aluW, posAluY + aluH / 2.)
            svgText "start" "middle" txtClass (posAluX + aluW + 7.) (posAluY + aluH / 2.) "Operand 1"
            reg (posX, posY) (getBits (beforeNum |> int))
            reg (posX, posY + sepY) (getBits op2Num)
            reg (posX, posY + sepY') (getBits destNum)
            svgIfTrue (rDest <> "") [ reg (posX, posY + sepY') (getBits destNum) ]
            makeLabel (posY + boxH / 2.) (rn.ToString())
            makeLabel (posY + sepY + boxH / 2.) "Operand 2"
            makeLabel (posY + sepY' + boxH / 2.) (sprintf "ALU out (%s)" rDest)
            colText txtClass (posCX + boxW / 2.) (posY - 1.) "C"
            colText txtClass (posCX - 2.) (posY + sepY' + 0.2 + boxH / 2.) "C"
        ] @ arrows)
    |> makeHtmlFromSVG


/// <summary>
/// Make an info button with associated hover tooltip.
/// </summary>
/// <param name="h"> horizontal char position for LH edge of button in editor </param>
/// <param name="v"> line number in editor buffer on which to place button (starting from 0 = top)</param>
/// <param name="buttonText"> label on button</param>
/// <param name="toolTipDOM"> DOM to display inside tooltip box </param>
let makeEditorInfoButtonWithTheme theme (clickable : bool) (h, v, orientation) 
                                  currentContentWidget editor editorFontSize 
                                  (buttonText : string) (toolTipDOM : HTMLElement) 
                                   =
    //Cmd.ofMsg DeleteAllContentWidgets |> ignore
    /// Ratio of char width / char size for editor buffer font.
    /// TODO: work this out properly from a test
    let editorFontWidthRatio = 0.6 // works OK for Fira Code Mono
    let name = buttonText.ToLower()
    let domID = sprintf "info-button-%s-%d" name v
    let tooltip = ELEMENT "DIV" [ sprintf "tooltip-%s" name ] [ toolTipDOM ]
    let dom =
        ELEMENT "BUTTON" [ sprintf "info-button-%s" name ] []
        |> INNERHTML buttonText
        |> ID domID
        |> STYLE ("margin-left", sprintf "%.0fpx" (editorFontWidthRatio * (float h + 2.0) * float (int editorFontSize)))
    dom.addEventListener_click (fun _ ->
        Browser.console.log (sprintf "Clicking button %s" buttonText) |> (fun _ -> createObj [])
        )
    //deleteContentWidget currentContentWidget editor domID // in some cases we may be updating an existing widget
    let newWidget = makeContentWidget domID dom editor currentContentWidget <| Exact(0, v)
    makeTooltip theme orientation clickable false domID tooltip
    newWidget

let lineTipsClickable = false

/// Drive the displayShiftDiagram function from a tooltip with correct parameters for given line
let makeShiftTooltip (h, v, orientation) (dp : DataPath, dpAfter : DataPath, uFAfter : DP.UFlags) (rn : RName) (shiftT : DP.ArmShiftType Option, alu : bool) (shiftAmt : uint32) (op2 : DP.Op2) =
    let bToi = function | true -> 1 | false -> 0
    let before = dp.Regs.[rn]
    let (after, uF) = DP.evalOp2 op2 dp
    let finalC = bToi dpAfter.Fl.C
    let final = match uFAfter.RegU with | [ rd ] -> rd.ToString(), (dpAfter.Regs.[rd] |> int) | _ -> "", 0
    let finalFWrite = uFAfter.CU
    let after' = after |> int32
    printfn "After': %d,%d" after after'
    printfn "Making shift tooltip"
    let diagram = displayShiftDiagram rn (before, bToi dp.Fl.C) (after', final, bToi uF.Ca, finalC, finalFWrite, alu) shiftT (shiftAmt |> int)
    Cmd.batch [
        Cmd.ofMsg DeleteAllContentWidgets 
        Cmd.ofMsg DeleteAllContentWidgets
        ("light", lineTipsClickable, h, (v + 1), orientation, diagram, "Shift") |> MakeEditorInfoButton |> Cmd.ofMsg 
        ]
