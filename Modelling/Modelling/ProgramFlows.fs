module ProgramFlows

open CalculatorTypesAST
open PrettyPrinter

type Flows = Set<string * string>


let rec get_var_statementA : (statementA -> List<string>) = fun stmA ->
    match stmA with
    | Number(x) -> []
    | Variable(s) -> [s]
    | Array(s, A) -> s::(get_var_statementA A)
    | Sum(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    | Diff(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    | Mul(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    | Div(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    | Pow(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    | Neg(A1) -> get_var_statementA A1;;


let rec get_var_statementB : (statementB -> List<string>) = fun stmB ->
    match stmB with
    |True -> []
    |False -> []
    |EagerAnd(B1, B2) -> (get_var_statementB B1) @ (get_var_statementB B2)
    |EagerOr(B1, B2) -> (get_var_statementB B1) @ (get_var_statementB B2)
    |ShortAnd(B1, B2) -> (get_var_statementB B1) @ (get_var_statementB B2)
    |ShortOr(B1, B2) -> (get_var_statementB B1) @ (get_var_statementB B2)
    |Negation(B) -> get_var_statementB B
    |Equality(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    |Inequality(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    |Greater(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    |GreaterOrEqual(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    |Less(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2)
    |LessOrEqual(A1, A2) -> (get_var_statementA A1) @ (get_var_statementA A2);;

let rec get_flows_statementC : (statementC -> List<string> -> List<string * string>) = fun program bound ->

    match program with
    | Assign(s, a) -> 
        let new_bound = bound @ get_var_statementA(a)
        List.map (fun x -> (x,s)) new_bound
    | AssignArray(s, a, b) -> 
        let new_bound = bound @ get_var_statementA(a) @ get_var_statementA(b)
        List.map (fun x -> (x,s)) new_bound
    | Commandline(c1, c2) -> 
        (get_flows_statementC c1 bound) @ (get_flows_statementC c2 bound)
    | Skip -> []
    | IfStat(gc) -> get_flows_statementC (DoStat(gc)) bound
    | DoStat(gc) -> 
        let mutable act_bound = bound
        let mutable act_flows = []
        let extractlist : (statementGC -> List<statementB * statementC>) = fun stmGC ->
            match stmGC with 
            | GC(l) -> l
        let gc_list = extractlist gc
        for (stmB, stmC) in gc_list do
            act_bound <- act_bound @ (get_var_statementB stmB) 
            act_flows <- act_flows @ (get_flows_statementC stmC act_bound)
        act_flows;;
    


