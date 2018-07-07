
(* 
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.CommonLex
    Description: Perform common part of parsing and define parse types
*)

/// functions and types for initial assembly parse
module CommonLex

    open CommonData
    open Expressions
    open Errors
    
    /// ARM execution conditions
    type Condition =

        | Ceq
        | Cne
        | Cmi
        | Cpl
        | Chi
        | Chs
        | Clo
        | Cls
        | Cge
        | Cgt
        | Cle
        | Clt
        | Cvs
        | Cvc
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions (example, add/change this is needed)
    type InstrClass = | DP | MEM | MISC | BRANCH | PSEUDO

    /// specification of set of instructions
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }




    /// result returned from instruction-specific module parsing
    /// an instruction class. If symbol definitions are found in a 
    /// symbol table then a complete parse will be output
    /// otherwise some fields will be None
    type Parse<'INS> = {
            /// value representing instruction. NB type varies with instruction class
            PInstr: Result<'INS,Errors.ParseError>
            /// name and value of `label defined on this line, if one is.
            PLabel: (string * Resolvable) option 
            /// number of bytes in memory taken up by this instruction
            ISize: uint32 
            /// number of data bytes in memory taken up by this instruction
            DSize: uint32 
            /// execution condition for instruction
            PCond: Condition
        }
    
    /// data given to instruction-specific parse function
    type LineData = {
        /// memory address this instruction is loaded. Must be word address
        LoadAddr: WAddr 
        /// name of label defined on this line, if one exists
        Label: string option
        /// table of symbols with defined values. 
        SymTab: SymbolTable
        /// opcode string
        OpCode: string
        /// string of all the operands
        Operands: string
    }

    let copyParse ld ins cond =
        let la = match ld.LoadAddr with | WA la -> la
        {
            PInstr = ins
            PLabel = ld.Label |> Option.map (fun lab -> (lab, la |> Ok)) 
            ISize = 4u
            DSize = 0u
            PCond = cond
        }

    let copyDefault (ld:LineData) cond =
        copyParse ld (Errors.makePE ``Undefined parse`` "" "") cond
 
        

    /// Strings with corresponding execution condition
    /// Note some conditions have multiple strings
    /// Note "" is a valid condition string (always execute condition)
    let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                    "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                    "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;"CC", Clo ; "CS", Chs
                    "NV",Cnv ; "AL",Cal ; "",Cal; "",Cal] |> Map.ofList

    /// list of all strings representing execution conditions
    /// includes ""
    let condStrings = 
        condMap
        |> Map.toList
        |> List.map fst
        |> List.distinct    

    /// generate all possible opcode strings for given specification
    /// each string is paired with info about instruction
    /// and the three parts of the opcode
    let opCodeExpand (spec: OpSpec) 
        //    opcode    class        root    suffix   instr cond
        : Map<string, InstrClass * (string * string * Condition)> =
        spec.Roots
        |> List.collect (fun r -> 
            spec.Suffixes
            |> List.collect (fun s -> 
                condStrings
                |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
                |> Map.ofList

