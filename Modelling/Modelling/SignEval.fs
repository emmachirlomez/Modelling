module SignEval

open CalculatorTypesAST
// Module evaluating conditions or statments and giving back their signs.

type Sign
    = Minus
    | Zero
    | Plus
    
type MemSign = Set<Sign>

type MemBool = Set<bool>

// Merges two sets of signs.
let MergeMemSigns : (MemSign -> MemSign -> MemSign) = fun m1 m2 ->
    Set.fold (fun s v -> s.Add v) m1 m2     

// Gets two sets of signs and returns their combination using a function.
// E.g. we have {+, -} * {-, 0}. We would like to know the answer will be {-, 0, +}.
// We can get this set by calling
//              EvaluateSignsOfFunction {+, -} {-, 0} (fun a b -> a * b)
// ofc instead of {+, -} we will have smth like      (set.empty.Add Plus).Add Minus
let EvaluateSignOfFunction : (MemSign -> MemSign -> (int -> int -> int) -> MemSign) = fun m1 m2 f ->
    let nrToSign n =
        if n < 0 then
            Minus
        elif n = 0 then
            Zero
        else
            Plus
    let sampleValsOfItem i =
        match i with
        | Minus -> [-1; -2; -3]
        | Zero -> [0]
        | Plus -> [1; 2; 3]

    let compute : (MemSign -> int -> int -> MemSign) = fun m_act v1 v2 ->
        try 
            let eval = f v1 v2
            m_act.Add (nrToSign eval)
        with 
            err -> m_act
        

    // Computes signs of f(a, b) where a has sign = s1 and b has sign = s2
    let combineValues : (Sign -> Sign -> MemSign) = fun s1 s2 ->
        let samples1 = sampleValsOfItem s1
        let samples2 = sampleValsOfItem s2

        let fun_for_fold2 : (MemSign -> int -> MemSign) = fun m_init v1 ->
            let newly_obtained = List.fold (fun m_act v2 -> compute m_act v1 v2) Set.empty samples2
            MergeMemSigns newly_obtained m_init

        List.fold fun_for_fold2 Set.empty samples1
    
    let combineSigns : (MemSign -> Sign -> MemSign) = fun mems_init s1 ->
        Set.fold (fun comb_act s2 -> MergeMemSigns (combineValues s1 s2) comb_act) mems_init m2

    Set.fold combineSigns Set.empty m1

// Returns possible bool typesmknowing the signs of the 2 variable
let EvaluateBoolOfFunction : (MemSign -> MemSign -> (int -> int ->bool) -> MemBool) = fun s1 s2 f ->
    let int_f a b = 
        if f a b then 
            1
        else
            -1
    let sign_to_bool s = 
        match s with
        | Plus -> true
        | Minus -> false
        | Zero -> failwith "Unexpected sign"
    let signs = EvaluateSignOfFunction s1 s2 int_f
    Set.map sign_to_bool signs;;


let EvaluateBoolOfBoolFunction : (MemBool -> MemBool -> (bool -> bool ->bool) -> MemBool) = fun s1 s2 f ->
    let iter_of_s2 : (MemBool -> bool -> MemBool) = fun m_act b1 ->
        Set.fold (fun m b2 -> m.Add(f b1 b2)) m_act s2
    Set.fold iter_of_s2 Set.empty s1;;

type VarMemory = Map<string, Sign>
type ArrMemory = Map<string, MemSign>
type Memory = VarMemory * ArrMemory


let rec EvaluateSignA : (statementA -> Memory -> MemSign) = fun stmA mem ->
    match stmA with 
    |Diff(a, b) ->
        EvaluateSignOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (-)
    |Mul(a, b) ->
        EvaluateSignOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (*)
    |Div(a, b) ->
        EvaluateSignOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (/)
    |Pow(a, b) ->
        EvaluateSignOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (pown)
    |Number(n) -> 
        if n < 0 then
            Set.empty.Add(Minus)
        elif n = 0 then
            Set.empty.Add(Zero)
        else
            Set.empty.Add(Plus)
    |Variable(s) -> Set.empty.Add(fst(mem).Item(s))
    |Array(s, a) -> snd(mem).Item(s)
    |Sum(a, b) ->
        EvaluateSignOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (+)
    |Neg(a) ->
        EvaluateSignOfFunction (Set.empty.Add(Zero)) (EvaluateSignA a mem) (-);;

let rec EvaluateSignB : (statementB -> Memory -> MemBool) = fun stmB mem ->
    match stmB with
    |Equality(a1, a2) ->
        EvaluateBoolOfFunction (EvaluateSignA a1 mem) (EvaluateSignA a2 mem) (=)
    |Inequality(a1, a2) ->
        EvaluateBoolOfFunction (EvaluateSignA a1 mem) (EvaluateSignA a2 mem) (<>)
    |Greater(a1, a2) ->
        EvaluateBoolOfFunction (EvaluateSignA a1 mem) (EvaluateSignA a2 mem) (>)
    |GreaterOrEqual(a1, a2) ->
        EvaluateBoolOfFunction (EvaluateSignA a1 mem) (EvaluateSignA a2 mem) (>=)
    |LessOrEqual(a1, a2) ->
        EvaluateBoolOfFunction (EvaluateSignA a1 mem) (EvaluateSignA a2 mem) (<=)
    |True -> Set.empty.Add(true)
    |False -> Set.empty.Add(false)
    |EagerAnd(b1, b2) -> 
        EvaluateBoolOfBoolFunction (EvaluateSignB b1 mem) (EvaluateSignB b2 mem) (&&)
    |EagerOr(b1, b2) -> 
        EvaluateBoolOfBoolFunction (EvaluateSignB b1 mem) (EvaluateSignB b2 mem) (||)
    |ShortAnd(b1, b2) -> 
        EvaluateBoolOfBoolFunction (EvaluateSignB b1 mem) (EvaluateSignB b2 mem) (&&)
    |ShortOr(b1, b2) -> 
        EvaluateBoolOfBoolFunction (EvaluateSignB b1 mem) (EvaluateSignB b2 mem) (||)
    |Negation(b) ->
        let set_of_bools = EvaluateSignB b mem
        Set.map not set_of_bools
    |Less(a, b) ->
        EvaluateBoolOfFunction (EvaluateSignA a mem) (EvaluateSignA b mem) (<)

let rec EvaluateSignC : (statementC -> Memory -> Memory list) = fun stmC mem ->
    match stmC with
    |Skip -> [mem]
    |Assign(s, stmA) ->
        let var_signs = EvaluateSignA stmA mem
        let modify_memory : (Sign -> Memory) = fun sign ->
            ((fst mem).Add(s, sign),snd mem)
        let set_of_memory = Set.map modify_memory var_signs
        Set.fold (fun l element -> element::l) [] set_of_memory
    |AssignArray(s, index, value) ->
        let var_signs = EvaluateSignA value mem
        let modify_memory : (Sign -> Memory) = fun sign ->
            let new_mem_sign = (snd mem).Item(s).Add(sign)
            (fst mem,(snd mem).Add(s, new_mem_sign))
        let set_of_memory = Set.map modify_memory var_signs
        Set.fold (fun l element -> element::l) [] set_of_memory

// Takes a set of memory and returns the set modified using statementC 
let passSignMemThroughStatementC : (Set<Memory> -> statementC -> Set<Memory>) = fun mem stateC -> 
    let set_of_list = Set.map (EvaluateSignC stateC) mem
    let list_of_mem = Set.fold (@) [] set_of_list
    Set.ofList list_of_mem;;
    

// Takes a set of memory and returns the set modified using statementB
let passSignMemThroughStatementB : (Set<Memory> -> statementB -> Set<Memory>) = fun mem stateB -> 
    Set.filter (fun m -> (EvaluateSignB stateB m).Contains(true)) mem;; 
    
   