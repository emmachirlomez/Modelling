module VariableInit

open VInitTypesAST

type Variables = Map<string,int>
type Arrays = Map<string, int list>

//TODO - placholder for the input of variables
let Variables:Variables = Map.ofList["a",10;"b",12]
let Arrays:Arrays = Map.ofList["A",[1;2;3]]

let temp = [1;2;3];;

let updateElement:(int -> int -> int list -> int list) = fun index element lst ->
    lst |> List.mapi (fun i v -> if i = index then element else v)
