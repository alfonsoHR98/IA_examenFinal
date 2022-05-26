// Carlos Alfonso Hernandez Rojas  170170
// Examen Final
// Inteligencia artificial 1
open System
open Busqueda

match Capitulo3.busquedaGrafo RutaSG.problema (AStar.estrategia RutaSG.h) (AStar.id RutaSG.h) with
    | None -> printfn "No se encontro la solución"
    | Some n -> printfn "Solucion %A" n
                printfn "Como lista %A" (Capitulo3.acciones n)  
                printfn "Profundidad: %i" (List.length (Capitulo3.acciones n) )