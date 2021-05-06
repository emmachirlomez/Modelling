module Task5

open System
open ProgramFlows
open Eval
open CalculatorTypesAST

type SecurityClassification = Map<string, string>
type SecurityLattice = List<string>

let rec readInitialVariablesMemory : (int ->  SecurityClassification -> SecurityLattice ->  SecurityClassification) = fun count mp lattice ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialVariablesMemory (count - 1) mp lattice
        printf "%s" ("What is the name of the " + count.ToString() + "th variable?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the security classification of '" + varname + "'?\n $ ")
        let value = string(Console.ReadLine())
        if (List.contains value lattice)  then
            rest_of_vars.Add(varname, value)
        else
            printfn "%s" "\nPlease input the correct classification!!!"
            readInitialVariablesMemory count mp lattice;;
   

let rec readInitialArraysMemory : (int ->  SecurityClassification -> SecurityLattice -> SecurityClassification) = fun count mp lattice ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialArraysMemory (count - 1) mp lattice
        printf "%s" ("What is the name of the #" + count.ToString() + " array?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the security classification of '" + varname + "?\n $ ")
        let value = string(Console.ReadLine())
        if (List.contains value lattice)  then
            rest_of_vars.Add(varname, value)
        else
            printfn "%s" "\nPlease input the correct classification!!!"
            readInitialVariablesMemory count mp lattice;;
   


let rec readInitialMemory : (unit -> (SecurityClassification * SecurityLattice)) = fun () ->
    printf "%s" ("Specify the security lattice? (separated by spaces in ascending order)\n $ ")
    let securityLattice = List.ofArray(string(Console.ReadLine()).Split(" "))

    printf "%s" ("How many variables do you want to initialise?\n $ ")
    let number_var = int(Console.ReadLine())
    let m1 = readInitialVariablesMemory number_var Map.empty securityLattice
    
    printf "%s" ("How many arrays do you want to initialise?\n $ ")
    let number_array = int(Console.ReadLine())
    let m2 = readInitialArraysMemory number_array m1 securityLattice
    
    (m2, securityLattice);;


let getAllowed : (SecurityClassification*SecurityLattice) -> Set<string*string> = fun (securityClass,lattice) ->
      let variableList = securityClass |> Map.toSeq |> Seq.map fst
      let mutable allowed = Set.empty
      for elem in variableList do
             let varValue = List.findIndex (fun x -> x.Equals(securityClass.Item elem)) lattice
             for elem2 in variableList do
                  if (varValue <= List.findIndex (fun x -> x.Equals(securityClass.Item elem2)) lattice) then
                      allowed <- Set.add (elem, elem2) allowed
                  else
                      allowed <- allowed
      allowed;;
          
 
let get_flows_and_violations : (statementC -> Set<string * string> * Set<string * string> * Set<string * string>) = fun program ->
    let allowed = getAllowed (readInitialMemory())
    let actual = Set.ofList(get_flows_statementC program [])
    let violations = Set.difference actual allowed
    (actual, allowed, violations);;
  
let print_nicely : (Set<string * string> -> unit) = fun s ->
    for (a, b) in s do 
        printf "%s" <| (a + "->" + b + " ");;

let printSecurity : (Set<string * string> * Set<string * string> * Set<string * string> -> unit) = fun (actual, allowed, violations) ->
    printfn "%s" <| "\nSecurity Analysis"
    printf "%s" <| "\n    * Actual:   "
    print_nicely actual
    printf "%s" <| "\n    * Allowed:   "
    print_nicely allowed
    printf "%s" <| "\n    * Violations:   "
    print_nicely violations
    if violations.Count > 0 then
        printfn "%s" <| "\n    * Verdict: Not secure"
    else
        printfn "%s" <| "\n    * Verdict: Secure";;
                  