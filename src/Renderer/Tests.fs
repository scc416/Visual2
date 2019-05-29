(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Tests
    Description: run automated tests using data geenrated by VisualRandomTestGen
*)

/// Run automatic emulator testing

module Tests
open System
open ExecutionTop
open Integration
open Errors
open EEExtensions
open Fable.Core
open Fable.Import
open Node.Exports
open CommonData
open Helpers

let projectDir = Refs.appDirName + @"/../"

let sampleDir = projectDir + @"app/samples/"