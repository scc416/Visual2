﻿(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Playground
    Description: File for self-contained test code and checking FABLE compiler bugs
*)

module TestLib

open EEExtensions
open Helpers
open CommonData
open ExecutionTop

type TbCheckResult = { Actual: uint32; Check: tbCheck; Spec: TbSpec}

type TestResult = {
    TestCoverage: int Set
    TestCycles: int64
    TestMsgs: string list
    TestLines: string list
    TestOk: bool
    TestNum: int
    TestName: string
    }

let maxTestLength = 100000L

let parseTbLine lNum (lin:string) =

    let apcsRegs rNumLst =
        let rnd = System.Random()
        rNumLst
        |> List.map register
        |> List.map (fun rn -> rn, rnd.Next(-100,100) |> uint32)

    let (|RESOLVE|_|) lst =
        let ops = 
            String.concat " " lst
            |> String.splitString [|","|] 
            |> Array.map String.trim
            |> Array.toList
        let parseLiteral  = function | LITERALNUMB (lit,"") -> [lit] | _ -> []
        let parseL = List.map parseLiteral ops
        if List.exists ((=) []) parseL then None
        else
            List.concat parseL |> Some
    /// Active pattern converts a string to uppercase - for case-insensitive matches
    let (|UPPER|_|) (name:string) = Some (name.ToUpper())
    let (|Defs|_|) words =
        match words with
        | [RegMatch (Ok rn) ; UPPER "IS" ; LITERALNUMB (lit,"")] -> (TbRegEquals(lNum, rn,lit)) |> Some
        | RegMatch (Ok rn) :: UPPER "PTR" :: RESOLVE lits -> (TbRegPointsTo(lNum, rn, 0u, lits)) |> Some
        | _ -> None
    let commentStrippedLine = 
        match String.split [|';'|] lin |> Array.toList with
        | [] -> ""
        | lin :: _ -> lin.Trim()
    match commentStrippedLine |> String.splitOnWhitespace |> Array.toList  with
    | [""] -> []
    | UPPER "IN" :: Defs tbSpec -> [Ok (TbIn, tbSpec)]
    | UPPER "OUT" :: Defs tbSpec -> [Ok (TbOut, tbSpec)]
    | UPPER "STACKPROTECT" :: _ -> [Ok(TbOut, TbStackProtected 0u)]
    | UPPER "DATAAREA" :: LITERALNUMB (lit,"") :: _ -> [Ok(TbIn, TbSetDataArea lit)]
    | UPPER "PERSISTENTREGS" :: RESOLVE lits -> [Ok(TbIn, APCS (apcsRegs (lits |> List.map int)))]
    | [UPPER "RANDOMISEINITVALS"]->  [Ok (TbIn, RandomiseInitVals)]
    | [UPPER "BRANCHTOSUB"; subName] -> [Ok (TbIn, BranchToSub subName)]
    | [UPPER "RELABEL" ; symName ; newSymName] -> [Ok (TbIn, Relabel( symName, newSymName))]
    | [UPPER "APPENDCODE"; num] -> [
        System.Int32.TryParse num 
        |> function | false,_-> Error (lNum, "AppendCode expects an integer representing the code block (#BLOCK) to append")
                    | true, num -> Ok (TbIn,AddCode(num,[]))
      ]
    | _ -> 
        [Error (lNum,"Parse Error in testbench")]




/// Process test specs in textual order linking allocated data addresses, and reordering specs where needed
/// Initstack - stack initial value.
/// Start - current data address for allocation
let linkSpecs initStack start specs =
    let addSpec (start,linkedSpecs) (inOut,spec) =
        match spec with
        | TbRegEquals(_lNum, rn,u) -> start, (inOut, spec) :: linkedSpecs
        | TbRegPointsTo(lNum, rn, _start, uLst) ->
            let rnOtherPntOpt = List.collect (function | _, TbRegPointsTo(_,rn',pnt,_) when rn' = rn -> [pnt] | _ -> []) linkedSpecs
            let start', pnt' =
                match rnOtherPntOpt with
                | pnt :: _ -> start, pnt
                | [] -> start + 4u*(uint32 uLst.Length), start
            start', (inOut, TbRegPointsTo(lNum, rn, pnt', uLst)) :: linkedSpecs
        | TbStackProtected _ -> start, (inOut, TbStackProtected initStack) :: linkedSpecs
        | TbSetDataArea u -> u, (inOut, TbSetDataArea u) :: linkedSpecs
        | APCS regs -> start, (inOut, APCS regs) :: linkedSpecs
        // no linkage required for these specs
        | RandomiseInitVals
        | AddCode _ 
        | Relabel _
        | BranchToSub _ -> start, (inOut,spec) :: linkedSpecs
    specs
    |> List.sortBy (function | _,RandomiseInitVals -> 0 | _ -> 1)
    |> List.fold addSpec (start,[]) 
    |> snd
    |> List.rev

/// Parse lines defining a single test.
/// Return Result is Test object, or list of line numbers and error messages.
let parseOneTest initStack dataStart testNum testName lines =

    let checkRes, tbLines =
        lines
        |> List.partition (snd >> String.trim >> String.startsWith ">>")
    
    let testLines,testErrors =
        tbLines
        |> List.collect (fun (i,lin) -> parseTbLine (i+1) lin)
        |> List.splitResult

    match testErrors with
    | [] ->
        let linkedLines = linkSpecs initStack dataStart testLines 
        { 
            TNum = testNum;
            TName = String.trim testName
            TAppendCode = []
            TRelabelSymbols = []
            Ins = List.collect (function | (TbIn,x) -> [x] | _ -> []) linkedLines
            Outs = List.collect (function | (TbOut,x) -> [x] | _ -> []) linkedLines
            CheckLines = (checkRes |> List.map snd)
            InitSP = initStack
            TestLines = lines |> List.map snd
        } |> Ok
    | errors -> Error errors

/// Parse lines defining a single code block.
/// Return Result is Test object.
let parseOneBlock blockNum blockName lines =
    match  lines with
    | [n,_hdr] -> Error <| [n, sprintf "Code block %d must have at least one line!" blockNum]
    | _ ->
        { 
            TNum = blockNum;
            TName = String.trim blockName
            TAppendCode = lines |> List.map snd
            TRelabelSymbols = []
            Ins = []
            Outs = []
            CheckLines = []
            InitSP = 0u
            TestLines = lines |> List.map snd
        } |> Ok
    

    
/// Parse testbench file returning as result list of Tests or errors
let parseTests initStack dStart lines =
    let parseChunk initStack dStart chunk =
        List.head chunk
        |> snd
        |> String.splitOnWhitespace 
        |> Array.toList
        |> (function | "#TEST" :: LITERALNUMB (n, testName) :: _ -> parseOneTest initStack dStart (int n) testName (List.tail chunk)
                     | "#BLOCK" :: LITERALNUMB (n, blockName) :: _ -> parseOneBlock (int n) blockName chunk
                     | x -> Error [1, sprintf "Can't parse test header '%A'" (List.truncate 2 x)])
    lines
    |> List.map String.trim
    |> List.indexed
    |> List.filter (snd >> (<>) "")
    |> List.tail
    |> List.chunkAt (snd >> (fun lin -> String.startsWith "#TEST" lin || String.startsWith "#BLOCK" lin))
    |> List.map (fun chunk -> parseChunk initStack dStart chunk)
    |> (fun lst ->
        let tests = List.filter (function | Ok {TAppendCode = []} | Error _ -> true | _ -> false) lst
        let getBlock n = List.tryFind (function | Ok {TNum=n'; TAppendCode = x} when x <> [] && n'=n -> true | _ -> false) lst
        let resolveBlocks (tst:Test) =
            let specs = 
                tst.Ins |> List.collect (
                    function | AddCode(n,body) -> 
                                getBlock n 
                                |> function | None | Some (Error _) -> [Error (0, sprintf "Can't find #BLOCK %d" n)]
                                            | Some (Ok tst) -> [AddCode(n, tst.TAppendCode) |> Ok]
                             | x -> [Ok x])
            let errors = List.errorList specs
            if  errors <> [] then Error errors else Ok {tst with Ins = List.okList specs   }  
        tests
        |> List.map (Result.bind resolveBlocks))




/// Make the initial dataPath (containing test inputs).
/// test: the Test.
/// lim: the initial LoadImage created from the memory image of the program being tested.
/// Return result because a test requiring a subroutine may fail at this stage.
let initTestDP  (lim: LoadImage) test =
    let dp = { 
                Fl = {
                        C=false
                        V=false
                        N=false
                        Z=false
                    }; 
                Regs=initialRegMap; 
                MM= lim.Mem
             }
    let rnd = System.Random()
    let setRand dp rn = Map.add rn (rnd.Next(-1000,1000) |> uint32) dp
    let rMap' = {dp with Regs = Map.add R13 test.InitSP dp.Regs}
    let ldSpecs = [
                    test.Ins |> List.map (fun sp -> TbIn,sp)
                    test.Outs |> List.map (fun sp -> TbOut,sp)
                  ] |> List.concat
    let addSpec dp (inout,spec) =
        match inout,spec with
        | TbOut, TbRegEquals(_, rn,u) -> Ok dp
        | TbIn, TbRegEquals(_, rn,u) -> Ok {dp with Regs = Map.add rn u dp.Regs}
        | tbio,TbRegPointsTo(_, rn, start, uLst) ->
            let mm' = ExecutionTop.addWordDataListToMem start dp.MM (uLst |> List.map Dat)
            let dp' = Map.add rn start dp.Regs
            Ok {dp with Regs = dp'; MM = (match tbio with | TbIn -> mm' | TbOut -> dp.MM)}
        | _, TbStackProtected _u -> Ok dp
        | _, APCS rLst -> 
            let rm = List.fold (fun regs (rn,u) -> Map.add rn u regs) dp.Regs rLst
            Ok {dp with Regs = rm}
        | _, TbSetDataArea u -> Ok dp
        | _, RandomiseInitVals -> Ok {dp with Regs = List.fold setRand dp.Regs [R0;R1;R2;R3;R4;R5;R6;R7;R8;R9;R10;R11;R12]}
        | _, BranchToSub subName -> 
            let subAddr = Map.tryFind subName lim.SymInf.SymTab
            match subAddr with
            | None -> Error (sprintf "Can't find subroutine '%s' required by test specification" subName)
            | Some subA ->
                dp
                |> Helpers.setRegRaw R15 subA
                |> Ok
        | _, _ -> Ok dp
    let addSpecBound dpRes spec = match dpRes with | Ok dp -> addSpec dp spec | e -> e
    List.fold addSpecBound (Ok rMap') ldSpecs
    |> Result.map (setReg R14 0xFFFFFFFCu)

/// Create a list of TbCheckResult from a test specification and output DataPath of the tested program.
/// test: the test specification (which will have generated program inputs)
/// dp: the Datapath to check against the specification outputs
let checkTestResults (test:Test) (outDp:DataPath) (subRet:bool) =
    let specs = test.Outs
    let isSubTest =
        List.exists (function | BranchToSub _ -> true | _ -> false) test.Ins
    let exitTest =
        //printfn "isSubTest=%A, subRet=%A" isSubTest subRet
        match isSubTest, subRet with
        | false, false | true, true -> []
        | false, true -> [0u, TbRet "\t>>- Subroutine return detected when normal program end expected", BranchToSub ""]
        | true, false -> [0u, TbRet "\t>>- Normal program end detected when subroutine return expected", BranchToSub ""]
        |> List.map (fun (v,c,sp) -> {Actual=v; Check=c; Spec=sp})
    let checkSpec spec =
        let checkOneLoc (ma,u) =
            match Map.tryFind (WA ma) outDp.MM with
            | Some (Dat u') when u = u' -> []
            | Some (Dat u') -> [{Actual = u; Check = TbMem (ma, Some u'); Spec = spec}]
            | _ -> [{ Actual = u; Check = TbMem (ma, None); Spec = spec}]
        match spec with
        | TbStackProtected sp when outDp.Regs.[R13] <> sp -> [{Actual=0u;Check=TbVal outDp.Regs.[R13];Spec= spec} ]
        | TbStackProtected sp ->
            Map.toList outDp.MM
            |> List.filter (fun (WA u, mm) -> u >= sp )
            |> List.collect (fun (WA u, mm) -> match mm with | Dat m -> [u,m] | CodeSpace -> [])
            |> List.map (fun (u,m) -> {Actual = 0u; Check = TbMem (u, Some m); Spec = spec})
        | TbRegEquals(lNum, rn, u) when outDp.Regs.[rn] = u -> []
        | TbRegEquals(lNum, rn, u) -> [{Actual =u; Check = TbVal outDp.Regs.[rn]; Spec = spec}]
        | TbRegPointsTo(_, rn, start, uLst) ->
            uLst
            |> List.indexed
            |> List.map (fun (n,u) ->  (start + uint32(4*n), u))
            |> List.collect checkOneLoc
        | APCS rLst -> 
            rLst
            |> List.collect (fun (rn,u') ->
                    match u' = outDp.Regs.[rn] with
                    | true -> [] 
                    | false -> [ {Actual=u' ; Check = TbVal outDp.Regs.[rn]; Spec = APCS [rn,u'] }])
        | _ -> []
    specs |> List.collect checkSpec
    |> (fun lst -> exitTest @ lst)



/// Return code transformed by test.
/// Transformation is idempotent.
let transformCodeByTest (code:string list) (test:Test) =
    let transformHdr = ";##TRANSFORM----------code appended by testbench---------"
    match code with
    | s :: _ when String.trim s = transformHdr -> code
    | _ -> 
        let labelChanges = List.collect (function | Relabel (ol,nl) -> [ol,nl] | _ -> []) test.Ins
        let changeLabels code (oldLab,newLab) = 
            List.map (fun lin -> if String.startsWith oldLab lin 
                                 then newLab + lin.[oldLab.Length..lin.Length-1] 
                                 else lin) code
        let appends = 
            match test.TAppendCode with
            | [] -> []
            | x -> transformHdr :: x
        let code' = List.fold changeLabels code labelChanges @ appends
        if code' <> code then
            let hdrComment = 
                ["; Code modified by testbench before testing!------"] @
                (labelChanges |> List.map (fun (ol,nl) -> sprintf ";Testbench has changed label '%s' to '%s'" ol nl)) @
                [";------student code (with label changes)----------"]
            hdrComment @ code'
        else
            code

/// Take assembler code and run test on it (transforming it if needed as per test).
/// Return Error if code parse fails.
/// Return Ok runInfo if parse succeeds.
let parseCodeAndRunTest (code:string list) (test:Test) = 
    let lim = reLoadProgram (transformCodeByTest code test)
    if lim.Errors <> []
    then 
        Error (sprintf "%A" lim.Errors)
    else
        let dp = initTestDP lim test 
        match dp with
        | Ok dp -> getRunInfoFromImageWithInits NoBreak lim dp.Regs dp.Fl Map.empty dp.MM |> Ok 
        | Error e -> Error e
        |> Result.map (asmStep maxTestLength)

/// Generate a text line explaining a TbCheckResult error.
/// Input: TbCheckResult.
/// Output: string containing error message.
let displayError {Actual = u: uint32 ; Check = check: tbCheck; Spec = spec:TbSpec}  =
    match check, spec with
    | TbVal spAct, TbStackProtected sp -> 
        sprintf "\t>>- Unbalanced Stack. SP: Actual: %d, Expected: %d" spAct sp
    | TbMem(adr,act), TbStackProtected sp -> 
        let actTxt = match act with None -> "None" | Some a -> sprintf "%d" a
        sprintf "\t>>- Caller Stack [%x] -> Actual: %s." adr actTxt
    | TbVal act, TbRegEquals(n, reg, v) -> 
        sprintf "\t>>- %A: Actual: %d, Expected: %d" reg act v
    | TbMem(adr,act), TbRegPointsTo(n, ptr, start, uLst) -> 
        let actTxt = match act with None -> "None" | Some a -> sprintf "%d" a
        let offset = int (adr - start)
        sprintf "\t>>- [%A,#%d] -> Actual: %s. expected: %d" ptr offset actTxt uLst.[offset/4] 
    | TbVal act, APCS [rn,exptd] -> sprintf "\t>>- Persistent Register %A -> Actual: %d. expected: %d" rn act exptd
    | TbRet errMess, _ -> errMess
    | _ -> failwithf "What?: inconsistent specs and check results"

/// Run a single test and generate pass/fail and text lines summarising result
/// Return (passed, messages) : (bool * string list).
/// passed: true if test passes
/// messages: list of lines suitable for testbench buffer insertion
/// dp: DataPath after test simulation ends.
let computeTestResults (test:Test) (dp:DataPath) =
    let subRet = dp.Regs.[R15] = 0xFFFFFFFCu
    let errorLines = 
        checkTestResults test dp subRet
        |> List.map displayError
    let resultLines =
        errorLines
        |> function | [] -> [sprintf ">\t\t>>; Test %d PASSED." test.TNum]
                    | errMess -> sprintf ">\t\t>>- Test %d FAILED." test.TNum :: errMess
    errorLines = [], resultLines

/// generate test results by simulating code (possibly transformed by test).
/// cache overall result for efficiency reasons
/// Note that simulation results are cached independently of this.
let runTestOnCode =
    let runTest' ((test, code): Test * string list) =
            let testRes = parseCodeAndRunTest code test
            let tr = {
                    TestCoverage = Set []
                    TestCycles = 0L
                    TestOk = false
                    TestLines = test.TestLines
                    TestMsgs = []
                    TestNum = test.TNum
                    TestName = test.TName
                }
            match testRes with
            | Error e -> { tr with TestMsgs = [e]}
            | Ok ri ->
                let ok, msgs = computeTestResults test (fst ri.dpCurrent)
                {   tr with
                        TestCoverage = ri.Coverage
                        TestCycles = ri.CyclesDone
                        TestOk = ok
                        TestMsgs = msgs
                }
    let rt = cacheLastN 200 runTest'
    fun test code -> rt (test,code) 
    