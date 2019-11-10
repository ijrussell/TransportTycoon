module Recursive

type Destination = Factory | Port | A | B
type Journey = Destination * int
type State = int * Destination list * Destination list * Journey * Journey * Journey

let evaluate (input:Journey) =
    let (d, a) = input
    match d with
    | A | B -> Some a
    | _ -> None

let getMaxValue items =
    match (items |> List.choose evaluate) with
    | [] -> 0
    | list -> list |> List.max

let processTruck (truck:Journey) cd cargo portCargo =
    match truck with
    | (d, a) when d = Factory && a = cd -> 
        match cargo with
        | A::tail -> ((Port, cd + 1), tail, portCargo) 
        | B::tail -> ((B, cd + 5), tail, portCargo) 
        | _ -> (truck, cargo, portCargo) 
    | (d, a) when d = Port && a = cd -> ((Factory, cd + 1), cargo, A::portCargo)
    | (d, a) when d = B && a = cd -> ((Factory, cd + 5), cargo, portCargo)
    | _ -> (truck, cargo, portCargo)

let processShip (ship:Journey) cd portCargo =
    match ship with
    | (d, a) when d = Port && a <= cd -> 
        match portCargo with
        | A::tail -> ((A, cd + 4), tail) 
        | _ -> ((d, cd), portCargo)
    | (d, a) when d = A && a = cd -> ((Port, cd + 4), portCargo) 
    | _ -> (ship, portCargo)

let rec tick state =
    let (i, cargo, portCargo, truck1, truck2, ship) = state
    if cargo <> [] || portCargo <> [] then
        let (truck1', cargo', portCargo') = (i, cargo, portCargo) |||> processTruck truck1 
        let (truck2', cargo'', portCargo'') = (i, cargo', portCargo') |||> processTruck truck2
        let (ship', portCargo''') = (i, portCargo'') ||> processShip ship 
        printfn "Iteration: %A - T1: %A - T2: %A - S: %A - Cargo: %A - PortCargo: %A" i truck1' truck2' ship' cargo'' portCargo'''
        tick (i + 1, cargo'', portCargo''', truck1', truck2', ship')
    else 
        [ truck1; truck2; ship ] |> getMaxValue

let run cargo =
    tick (0, cargo, [], (Factory, 0), (Factory, 0), (Port, 0))

run [A;A;B;A;B;B;A;B] |> printfn "%A" 

