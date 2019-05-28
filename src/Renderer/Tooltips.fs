(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tooltips
    Description: Code to implement tooltips and dynamic graphical popups
*)

/// F# References to static parts of renderer DOM
module Tooltips

open Fable.Import
open Fable.Import.Browser
open Fable.Core.JsInterop
open Microsoft.FSharp.Collections
open EEExtensions
open Fable.Helpers.React
open Fable.Core
open Refs
open CommonData

let deleteContentWidget currentTabWidgets editor name =
    match Map.tryFind name currentTabWidgets with
    | None -> ()
    | Some w -> editor?removeContentWidget w |> ignore

/// delete all content widgets
let deleteAllContentWidgets currentTabWidgets editor =
    Array.iter (deleteContentWidget currentTabWidgets editor) (Map.keys currentTabWidgets)

type TooltipsProps =
    | Content of string
    | Animation of string
    | Arrow of bool
    | Theme of string
    | Distance of int
    | HideOnClick of bool
    | Placement of string
    | Delay of int * int
    
let inline tooltips (props: TooltipsProps list) 
                    (elLst: React.ReactElement list) : React.ReactElement =
    ofImport "default" 
             "@tippy.js/react" 
             (keyValueList CaseRules.LowerFirst props) 
             elLst

let basicTooltipsPropsLst =
    [ Animation "fade"
      Arrow true
      Animation "fade"
      Theme "bootstrap"
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

