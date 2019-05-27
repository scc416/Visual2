(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Files
    Description: IDE file Load and store via electrom main process dialogs
*)

/// implement load and save of assembler files

module Files2
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Exports
open Fable.PowerPack
open EEExtensions

open Fable.Import.Browser


open Refs
open Fable
open Tabs

open CommonData
open ExecutionTop

//*************************************************************************************
//                           FILE LOAD AND STORE
//*************************************************************************************

