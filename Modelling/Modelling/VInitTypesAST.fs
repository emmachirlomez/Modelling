module VInitTypesAST


type assign =
    | Variable of (string * int)
    | ArrayV of (string * int list)

type assignments = assign list
    