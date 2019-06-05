module MenuBar

open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Base
open Refs
open Views
open Elmish
open Integration
open Tests
open Testbench

let runExtPage url () =
    electron.shell.openExternal url |> ignore

let display runMode =
    match runMode with
    | ExecutionTop.ResetMode -> "ResetMode"
    | ExecutionTop.FinishedMode _ -> "FinishedMode"
    | ExecutionTop.ActiveMode _ -> "ActiveMode"
    | ExecutionTop.ParseErrorMode -> "ParseErrorMode"
    | ExecutionTop.RunErrorMode _ -> "RunErrorMode"
    
/// Wrap an action so that it can only happen if simulator is stopped.
/// Converts unit -> unit into obj. Must be called as fun () -> interlock actionName action.
/// Suitable for use as JS callback.
let interlock (actionName : string) debugLevel runMode dispatch msg =
    let action = fun _ -> msg |> dispatch
    if debugLevel > 0 then printf "Interlock : runMode=%A" (display runMode)
    match runMode with
    | ExecutionTop.ResetMode
    | ExecutionTop.ParseErrorMode -> 
        action ()
    | _ -> 
        ((sprintf "Can't %s while simulator is running <br> <br>Reset and %s<br>" actionName actionName), msg)
        |> ResetEmulatorDl
        |> UpdateDialogBox 
        |> dispatch

 /// Wrap an action so that it can only happen if simulator is stopped.
 /// Operates on (Unit->Unit) to make (Unit->Unit).
 /// Suitable for use as action in menu.
let interlockAction (actionName : string) debugLevel runMode dispatch msg = (fun () ->
    interlock actionName debugLevel runMode dispatch msg|> ignore
    )

(****************************************************************************************************
 *
 *                                      MENU OPERATIONS
 *
 ****************************************************************************************************)

let loadDemo (editors : Map<int, Editor>) =
    let sampleFileName = Tests.sampleDir + "karatsuba.s"
    let txt = 
        Node.Exports.fs.readFileSync (sampleFileName, "utf8")
    let newEditor = 
        { DefaultValue = txt 
          FileName = Option.None
          FilePath = Option.None 
          IEditor = Option.None
          Saved = true }
    let newId = Refs.uniqueTabId editors
    let newEditors= Map.add newId newEditor editors
    { Editors = newEditors
      TabId = newId }

(****************************************************************************************************
 *
 *                                  MENU HELPER FUNCTIONS
 *
 ****************************************************************************************************)


let menuSeparator =
    let sep = createEmpty<MenuItemOptions>
    sep.``type`` <- Some Separator
    sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : unit -> unit) =
    let handlerCaster f = System.Func<MenuItem, BrowserWindow, unit> f |> Some
    let item = createEmpty<MenuItemOptions>
    item.label <- Some label
    item.accelerator <- accelerator
    item.click <- handlerCaster (fun _ _ -> iAction())
    item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
    let item = makeItem label accelerator id
    item.role <- U2.Case1 role |> Some
    item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
    let item = makeItem label accelerator action
    item.visible <- Some cond
    item

/// Make a new menu from a a list of menu items
let makeMenu (name : string) (table : MenuItemOptions list) =
    let subMenu = createEmpty<MenuItemOptions>
    subMenu.label <- Some name
    subMenu.submenu <-
        table
        |> ResizeArray<MenuItemOptions>
        |> U2.Case2 |> Some
    subMenu


(****************************************************************************************************
 *
 *                                         MENUS
 *
 ****************************************************************************************************)
let fileMenu (dispatch : (Msg -> Unit)) id debugLevel runMode =
    makeMenu "File" [
            makeItem "New" (Some "CmdOrCtrl+N") (interlockAction "make new file tab" debugLevel runMode dispatch NewFile)
            menuSeparator
            makeItem "Save" (Some "CmdOrCtrl+S") (interlockAction "save file" debugLevel runMode dispatch SaveFile)
            makeItem "Save As" (Some "CmdOrCtrl+Shift+S") (interlockAction "save file" debugLevel runMode dispatch SaveAsFileDialog)
            makeItem "Open" (Some "CmdOrCtrl+O") (interlockAction "open file" debugLevel runMode dispatch OpenFileDialog)
            menuSeparator
            makeItem "Close" (Some "CmdOrCtrl+W") (interlockAction "close file" debugLevel runMode dispatch (AttemptToDeleteTab id))
            menuSeparator
            makeItem "Quit" (Some "CmdOrCtrl+Q") (interlockAction "quit" debugLevel runMode dispatch AttemptToExit) 
        ]

let editMenu (dispatch : (Msg -> Unit)) debugLevel runMode =
    makeMenu "Edit" [
        makeItem "Undo" (Some "CmdOrCtrl+Z") (fun _ -> UndoEditor |> dispatch )
        makeItem "Redo" (Some "CmdOrCtrl+Shift+Z") (fun _ -> RedoEditor |> dispatch )
        menuSeparator
        makeRoleItem "Cut" (Some "CmdOrCtrl+X") MenuItemRole.Cut
        makeRoleItem "Copy" (Some "CmdOrCtrl+C") MenuItemRole.Copy
        makeRoleItem "Paste" (Some "CmdOrCtrl+V") MenuItemRole.Paste
        menuSeparator
        makeItem "Select All" (Some "CmdOrCtrl+A") (fun _ -> SelectAllEditor |> dispatch )
        menuSeparator
        makeItem "Find" (Some "CmdOrCtrl+F") (fun _ -> FindEditor |> dispatch)
        makeItem "Replace" (Some "CmdOrCtrl+H") (fun _ -> FindAndReplaceEditor |> dispatch)
        menuSeparator
        makeItem "Increase Font Size" (Some "CmdOrCtrl+.") (fun _ -> IncreaseFontSize |> dispatch )
        makeItem "Decrease Font Size" (Some "CmdOrCtrl+,") (fun _ -> DecreaseFontSize |> dispatch )
        makeItem "Preferences" Core.Option.None (interlockAction "show preferences tab" debugLevel runMode dispatch SelectSettingsTab)
    ]

let viewMenu() =
        let devToolsKey = if Node.Globals.``process``.platform = NodeJS.Platform.Darwin then "Alt+Command+I" else "Ctrl+Shift+I"
        makeMenu "View" [
            makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.Togglefullscreen
            menuSeparator
            makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.Zoomin
            makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.Zoomout
            makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.Resetzoom
            menuSeparator
            //makeCondItem (debugLevel > 0) "Toggle Dev Tools" (Some devToolsKey) (electron.remote.getCurrentWebContents()).toggleDevTools
            makeItem "Toggle Dev Tools" (Some devToolsKey) (electron.remote.getCurrentWebContents()).toggleDevTools
        ]

let popupMenu (items) =
    let menu = electron.remote.Menu.Create()
    items
    |> List.map electron.remote.MenuItem.Create
    |> List.iter menu.append
    menu.popup (electron.remote.getCurrentWindow())
    ()

let runSingleTest editors (disptach : Msg -> unit) =
    let decorations, cmd, testLst = getTestList editors
    match testLst with
    | [] -> 
        "Can't find any tests. Have you loaded a valid testbench?"
        |> Alert
        |> UpdateDialogBox
    | lst -> 
        let testLst = 
            List.map (fun (test : ExecutionTop.Test) ->
                let name = sprintf "Step code with initial data from Test %d" test.TNum
                let actFun = fun () -> test |> StartTest |> disptach
                makeItem name Core.None actFun) lst
        testLst
        |> PopupMenu

let testMenu (dispatch : (Msg -> Unit)) debugLevel runMode editors =
        let runToBranch() = ()
        let menu = electron.remote.Menu.Create()
        let runTo cond = (cond, System.Int64.MaxValue) |> RunEditorTab |> dispatch
        makeMenu "Test" [
            makeItem "Step <-" (Some "F3") (fun () -> StepCodeBackBy 1L |> dispatch)
            makeItem "Step ->" (Some "F4") (fun () -> StepCode |> dispatch )
            makeItem "Step to next call" (Some "F5") (fun () -> runTo ExecutionTop.ToSubroutine )
            makeItem "Step to next return" (Some "F6") (fun () -> runTo ExecutionTop.ToReturn )
            makeItem "Step forward by" Core.Option.None (fun () -> UpdateDialogBox StepDl |> dispatch )
            makeItem "Step back by" Core.Option.None (fun () -> UpdateDialogBox StepBackDl |> dispatch )
            menuSeparator
            makeItem "Step into test" Core.Option.None (fun () -> dispatch |> runSingleTest editors |> dispatch )
            makeItem "Run all tests" Core.Option.None (interlockAction "Testbench" debugLevel runMode dispatch RunTestBenchOnCode)
        ]

let helpMenu dispatch debugLevel runMode =
        let newDialogBox = Alert (sprintf "<h4>VisUAL2 ARM Simulator v%s</h4> " Refs.appVersion +
                               "(c) 2018, Imperial College <br> Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
                               " (F# reimplementation), with special mention to Thomas Carrotti," +
                               " Lorenzo Silvestri, and HLP Team 10")
        makeMenu "Help" (
            [
                makeItem "UAL instruction guide" Core.Option.None (runExtPage <| visualDocsPage "guide#content")
                makeItem "VisUAL2 web pages" Core.Option.None (runExtPage <| visualDocsPage "")
                makeItem "Testbenches" Core.Option.None (runExtPage <| visualDocsPage "testbench")
                makeItem "Official ARM documentation" Core.Option.None (runExtPage "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0234b/i1010871.html")
                menuSeparator
                makeItem "Load complex demo code" Core.Option.None (interlockAction "load code" debugLevel runMode dispatch LoadDemoCode)
                makeCondItem (debugLevel > 0) "Run dev tools FABLE checks" Core.Option.None (interlockAction "FABLE checks" debugLevel runMode dispatch RunTestBench)
                makeCondItem (debugLevel > 0) "Run Emulator Tests" Core.Option.None (interlockAction "run tests" debugLevel runMode dispatch RunAllEmulatorTests)
                menuSeparator
                makeItem "About" Core.option.None (fun _ -> (newDialogBox |> UpdateDialogBox |> dispatch ))
            ])


/// Make all app menus
let mainMenu info
             debugLevel
             runMode
             (dispatch : (Msg -> Unit))=
    let template =
        ResizeArray<MenuItemOptions> [
            fileMenu dispatch info.TabId debugLevel runMode
            editMenu dispatch debugLevel runMode
            viewMenu()
            helpMenu dispatch debugLevel runMode
            testMenu dispatch debugLevel runMode info.Editors
        ]
    template
    |> electron.remote.Menu.buildFromTemplate
    |> electron.remote.Menu.setApplicationMenu
        