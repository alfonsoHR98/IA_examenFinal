// Carlos Alfonso Hernandez Rojas  170170
// Examen Final
// Inteligencia artificial 1
namespace Busqueda 

module AStar =
    let estrategia<'s,'a when 's: comparison > h = 
        {
            agregar     = fun map n -> Map.add(n.costo_ruta + h n, n.estado) n map
            siguiente   = fun map ->
                                match Map.tryFindKey (fun _ _ -> true) map with
                                | None -> None
                                | Some k -> Some(map.[k], Map.remove k map)
            inicializar = fun n -> Map.add(0.0,n.estado) n Map.empty
        } : estrategia<'s,'a,Map<float * 's,nodo<'s,'a>>>
    let id h n = n.costo_ruta + h n, n.estado