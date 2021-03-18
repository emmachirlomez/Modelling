module File1

open CalculatorTypesAST
open System.IO

let node1 = 0
let node2 = -1

type Edge = 
    | AssignEdge of (int * statementC * int)
    | CheckerEdge of (int * statementB * int)

// startnode, finalnode, statementC, nr -> 
//edges((a := 10; b := 20) 
//edges((a := 10; b := 20), 1, 4, 10) =
// 1 --(a:=10)--> 11
// 11 --(b:=20)---> 4
// returns {(q1, a:10, q2),(q2, b:=10, q3)}
let rec edges : (statementC -> int -> int -> int -> Edge list* int) = fun program startId endId nrNodes ->
   
    //edges program0(node1,???) + egdes programtail (node1, node2) 

    match program with
    | Assign(s, a) -> ([AssignEdge(startId, Assign(s, a) , endId)], 0)
    | AssignArray(s, a, b) -> ([AssignEdge(startId, AssignArray(s, a, b) , endId)],0)
    | Commandline(c1, c2) -> 
                    let middlenode = nrNodes + 1
                    let (edges1, usedNodes1) = edges c1 startId middlenode (nrNodes + 1)
                    let (edges2, usedNodes2) = edges c2 middlenode endId (nrNodes + 1 + usedNodes1)
                    (edges1 @ edges2, usedNodes1 + usedNodes2 + 1)
    ;;

//let edges : (statementB -> int -> int -> int -> Edge list * int ) = fun program startId endId nrNodes ->
    



//let str = "digraph program_graph {rankdir=LR;\n
//           node [shape = circle]; q▷;\n
//           node [shape = doublecircle]; q◀; \n
//           node [shape = circle]\n"

//let pg = new System.IO.StreamWriter("../../../Test.gv")

//pg.Write(str)
//pg.Close()
