module ProgramGraph

open CalculatorTypesAST
open PrettyPrinter

let node1 = 0
let node2 = -1

type Edge = 
    | AssignEdge of (int * statementC * int)
    | CheckerEdge of (int * statementB * int)

let ExtractListFromGC(GC a) = a;;

// startnode, finalnode, statementC, nr -> 
//edges((a := 10; b := 20) 
//edges((a := 10; b := 20), 1, 4, 10) =
// 1 --(a:=10)--> 11
// 11 --(b:=20)---> 4
// returns {(q1, a:10, q2),(q2, b:=10, q3)}
let rec edgesC : (bool -> statementC -> int -> int -> int -> Edge list* int) = fun determ program startId endId nrNodes ->
   
    //edges program0(node1,???) + egdes programtail (node1, node2) 

    match program with
    | Assign(s, a) -> ([AssignEdge(startId, Assign(s, a) , endId)], 0)
    | AssignArray(s, a, b) -> ([AssignEdge(startId, AssignArray(s, a, b) , endId)],0)
    | Commandline(c1, c2) -> 
                    let middlenode = nrNodes + 1
                    let (edges1, usedNodes1) = edgesC determ c1 startId middlenode (nrNodes + 1)
                    let (edges2, usedNodes2) = edgesC determ c2 middlenode endId (nrNodes + 1 + usedNodes1)
                    (edges1 @ edges2, usedNodes1 + usedNodes2 + 1)
    | IfStat(gc) -> edgesGC determ (ExtractListFromGC gc) startId endId nrNodes
    | DoStat(gc) -> 
                    let (edges1, usedNodes1) = edgesGC determ (ExtractListFromGC gc) startId startId nrNodes
                    
                    let new_edge = CheckerEdge(startId, negationGC (ExtractListFromGC gc), endId)
                    (new_edge::edges1, usedNodes1)
    | Skip -> ([AssignEdge(startId, Skip, endId)], 0)

and negationGC: ((statementB * statementC) list -> statementB) = fun gc ->
    match gc with 
    | [] -> True
    | [(b, c)] -> Negation b
    | (b, c)::t -> EagerAnd ((Negation b), (negationGC t))

and edgesGC : (bool -> (statementB * statementC) list -> int -> int -> int -> Edge list * int) = fun determ program startId endId nrNodes ->
    match program with 
    | [] -> ([], 0)
    | [(b, c)] ->
        let middlenode = nrNodes + 1
        let (edges1, usedNodes1) = ([CheckerEdge(startId, b, middlenode)], 0)
        let (edges2, usedNodes2) = edgesC determ c middlenode endId (nrNodes + 1 + usedNodes1)
        (edges1 @ edges2, usedNodes1 + usedNodes2 + 1)
    | (b, c)::t ->
        let (edges1, nodes1) = edgesGC determ [(b, c)] startId endId nrNodes
        if determ = false then
            let (edges2, nodes2) = edgesGC determ t startId endId (nrNodes + nodes1)
            (edges1 @ edges2, nodes1 + nodes2)
        else
            let new_t = List.map (fun (b2, c2) -> (ShortAnd(b2, (Negation b)), c2)) t
            
            let (edges2, nodes2) = edgesGC determ new_t startId endId (nrNodes + nodes1)
            (edges1 @ edges2, nodes1 + nodes2);;

let GVGenerator : (bool -> statementC -> string -> string) = fun determ program fileName -> 
    let str = "digraph program_graph {rankdir=LR;\n" +
              "node [shape = circle]; q▷;\n" +
              "node [shape = doublecircle]; q◀;\n" + 
              "node [shape = circle]\n"
    let (edges, nrNodes) = edgesC determ program -1 -2 0
    let qName : (int -> string) = fun n ->
        match n with 
        | -1 -> "q▷"
        | -2 -> "q◀"
        | x  -> "q" + x.ToString()
    let EdgeToString : (Edge -> string) = fun e -> 
        match e with 
        | AssignEdge(startId, expression, endId) ->
            (qName startId) + " -> " + (qName endId) + "[label = \"" + (prettyPrinterC expression).Head + "\"];\n"
        | CheckerEdge(startId, expression, endId) ->
            (qName startId) + " -> " + (qName endId) + "[label = \"" + (prettyPrinterB expression) + "\"];\n"
    let listOfStrings = List.map EdgeToString edges
    let ans = List.fold (+) "" listOfStrings
        
    let pg = new System.IO.StreamWriter("../../../" + fileName)
    pg.Write(str + ans + "}\n")
    pg.Close()
    str + ans + "}\n";;