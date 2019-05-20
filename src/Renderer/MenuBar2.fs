module MenuBar2

open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Node.Base
open Refs
open Settings
open Tabs
open EEExtensions
open Files2
open Tabs2


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
let interlock (actionName : string) (action : Unit -> Unit) = (
        if debugLevel > 0 then printf "Interlock : runMode=%A" (display runMode)
        let actIfConfirmed buttonNum =
            printf "button %A" buttonNum
            match buttonNum with
            | false -> ()
            | _ -> Integration.resetEmulator(); action()
        match Refs.runMode with
        | ExecutionTop.ResetMode
        | ExecutionTop.ParseErrorMode -> action() :> obj
        | _ -> showVexConfirm (sprintf "Can't %s while simulator is running <br> <br>Reset and %s<br>" actionName actionName) actIfConfirmed :> obj
    )
 /// Wrap an action so that it can only happen if simulator is stopped.
 /// Operates on (Unit->Unit) to make (Unit->Unit).
 /// Suitable for use as action in menu.
let interlockAction (actionName : string) (action : Unit -> Unit) = (fun () ->
    interlock actionName action |> ignore
    )

(****************************************************************************************************
 *
 *                                      MENU OPERATIONS
 *
 ****************************************************************************************************)


 // Popup windows

let makePrefsWindow() =
    let url = "prefs.html"
    let mutable prefsWindow = (Core.Option.None : BrowserWindow option)

    // Register click on the About menu

    let options = createEmpty<BrowserWindowOptions>
    let webPrefs = createEmpty<WebPreferences>
    webPrefs?nativeWindowOpen <- Some true
    options?toolbar <- Some false
    options.resizable <- Some false
    options.show <- Some true
    options.height <- Some 600.
    options.width <- Some 400.
    options.modal <- Some true
    options.parent <- Some(electron.remote.getCurrentWindow())
    options.webPreferences <- Some webPrefs


    let prefs = electron.remote.BrowserWindow.Create(options)
    // For to remove the menu of the window
    prefs.setMenu (unbox null)

    prefs.on ("closed", unbox (fun () ->
        // Dereference the about window object.
        prefsWindow <- Option.None
    )) |> ignore

    prefs.loadURL (Node.Exports.path.join ("file://", Node.Globals.__dirname, url))

    prefsWindow <- prefs |> Some



 /// Load the node Buffer into the specified tab
let loadFileIntoTab tId (fileData : Node.Buffer.Buffer) =
    if currentFileTabId = tId then
        Integration.resetEmulator()
    let editor = editors.[tId]
    editor?setValue (fileData.toString ("utf8")) |> ignore
    setTabSaved tId

    


let loadDemo (editors : Map<int, Editor>) : ( Map<int, Editor> * int) =
    let sampleFileName = Tests.sampleDir + "karatsuba.s"
    let txt = 
        Node.Exports.fs.readFileSync (sampleFileName, "utf8")
    let newEditor = 
        { FileName = Option.None
          FilePath = Option.None 
          Saved = true
          EditorText = txt }
    let newId = uniqueTabId editors
    let newEditors= Map.add newId newEditor editors
    newEditors, newId



let showQuitMessage (callBack : bool -> unit) =
    let mess = "You have unsaved changes. Are you sure you want to exit and lose changes?"
    let buttons = [ "Save"; "Exit without saving" ]
    Refs.showVexConfirm mess callBack


/// Check if there is any unsaved info. Display dialog asking for confirmation if there is.
/// Otherwise exit.
let ExitIfOK() =
    let close() = electron.ipcRenderer.send "doClose" |> ignore
    let callback (result : bool) =
        match result with
        | false -> ()
        | true -> close()
    let tabL = Tabs.unsavedTabs()
    if tabL <> [] then
        showQuitMessage callback
    else close()

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

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
    let item = makeItem label accelerator id
    item.role <- U2.Case1 role |> Some
    item.visible <- Some cond
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
let fileMenu id (dispatch : (Msg -> Unit)) =
    makeMenu "File" [
            makeItem "New" (Some "CmdOrCtrl+N") (interlockAction "make new file tab" (fun _ -> Refs.NewFile |> dispatch))
            menuSeparator
            makeItem "Save" (Some "CmdOrCtrl+S") (interlockAction "save file" (fun _ -> Refs.SaveFile |> dispatch ))
            makeItem "Save As" (Some "CmdOrCtrl+Shift+S") (interlockAction "save file" (fun _ -> Refs.SaveAsFileDialog |> dispatch ))
            makeItem "Open" (Some "CmdOrCtrl+O") (interlockAction "open file" (fun _ -> Refs.OpenFileDialog |> dispatch ))
            menuSeparator
            makeItem "Close" (Some "CmdOrCtrl+W") (interlockAction "close file" (fun _ -> Refs.DeleteTab id |> dispatch ))
            menuSeparator
            makeItem "Quit" (Some "CmdOrCtrl+Q") ExitIfOK
        ]



let editMenu (dispatch : (Msg -> Unit)) =
    makeMenu "Edit" [
        //makeItem "Undo" (Some "CmdOrCtrl+Z") (fun _ -> EditorUndo |> dispatch )
        //makeItem "Redo" (Some "CmdOrCtrl+Shift+Z") Files.editorRedo
        menuSeparator
        makeRoleItem "Cut" (Some "CmdOrCtrl+X") MenuItemRole.Cut
        makeRoleItem "Copy" (Some "CmdOrCtrl+C") MenuItemRole.Copy
        makeRoleItem "Paste" (Some "CmdOrCtrl+V") MenuItemRole.Paste
        menuSeparator
        makeRoleItem "Select All" (Some "CmdOrCtrl+A") MenuItemRole.Selectall
        //menuSeparator
        //makeItem "Find" (Some "CmdOrCtrl+F") Files.editorFind
        //makeItem "Replace" (Some "CmdOrCtrl+H") Files.editorFindReplace
        menuSeparator
        makeItem "Increase Font Size" (Some "CmdOrCtrl+.") (fun _ -> IncreaseFontSize |> dispatch )
        makeItem "Decrease Font Size" (Some "CmdOrCtrl+,") (fun _ -> DecreaseFontSize |> dispatch )
        makeItem "Preferences" Core.Option.None (interlockAction "show preferences tab" (fun _ -> SelectSettingsTab |> dispatch ))
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

let testMenu() =
        let runToBranch() = ()
        let menu = electron.remote.Menu.Create()
        let runSteps() =
            showVexValidatedPrompt "steps forward" validPosInt (int64 >> (Integration.runEditorTab ExecutionTop.NoBreak)) "Number of steps forward"
        let runStepsBack() =
            showVexValidatedPrompt "steps back" validPosInt (int64 >> (Integration.stepCodeBackBy)) "Number of steps back"
        let runSingleTest() =
            match Testbench.getTestList() with
            | [] -> showVexAlert "Can't find any tests. Have you loaded a valid testbench?"
            | lst -> popupMenu (List.map (fun (test : ExecutionTop.Test) ->
                        let name = sprintf "Step code with initial data from Test %d" test.TNum
                        let actFun = fun () -> Integration.startTest test
                        makeItem name Core.None actFun) lst)
        let runTo cond () = Integration.runEditorTab cond System.Int64.MaxValue
        makeMenu "Test" [
            makeItem "Step <-" (Some "F3") Integration.stepCodeBack
            makeItem "Step ->" (Some "F4") Integration.stepCode
            makeItem "Step to next call" (Some "F5") (runTo ExecutionTop.ToSubroutine)
            makeItem "Step to next return" (Some "F6") (runTo ExecutionTop.ToReturn)
            makeItem "Step forward by" Core.Option.None runSteps
            makeItem "Step back by" Core.Option.None runStepsBack
            menuSeparator
            makeItem "Step into test" Core.Option.None (interlockAction "Test" runSingleTest)
            makeItem "Run all tests" Core.Option.None (interlockAction "Testbench" Integration.runTestbenchOnCode)
        ]


let helpMenu dispatch =
        makeMenu "Help" (
            [
                makeItem "UAL instruction guide" Core.Option.None (runExtPage <| visualDocsPage "guide#content")
                makeItem "VisUAL2 web pages" Core.Option.None (runExtPage <| visualDocsPage "")
                makeItem "Testbenches" Core.Option.None (runExtPage <| visualDocsPage "testbench")
                makeItem "Official ARM documentation" Core.Option.None (runExtPage "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0234b/i1010871.html")
                menuSeparator
                makeItem "Load complex demo code" Core.Option.None (interlockAction "load code" (fun _ -> Refs.LoadDemoCode |> dispatch))
                makeCondItem (debugLevel > 0) "Run dev tools FABLE checks" Core.Option.None (interlockAction "FABLE checks" Integration.runTestbench)
                makeCondItem (debugLevel > 0) "Run Emulator Tests" Core.Option.None (interlockAction "run tests" Tests.runAllEmulatorTests)
                menuSeparator
                makeItem "About" Core.option.None (fun () ->
                    printfn "Directory is:%s" (Stats.dirOfSettings())
                    showVexAlert (sprintf "<h4>VisUAL2 ARM Simulator v%s</h4> " Refs.appVersion +
                                "(c) 2018, Imperial College <br> Acknowledgements: Salman Arif (VisUAL), HLP 2018 class" +
                                " (F# reimplementation), with special mention to Thomas Carrotti," +
                                " Lorenzo Silvestri, and HLP Team 10"))
            ])


/// Make all app menus
let mainMenu id (dispatch : (Msg -> Unit))=
    let template =
        ResizeArray<MenuItemOptions> [
            fileMenu id dispatch
            editMenu dispatch
            viewMenu()
            helpMenu dispatch
            testMenu()
        ]
    template
    |> electron.remote.Menu.buildFromTemplate
    |> electron.remote.Menu.setApplicationMenu
        