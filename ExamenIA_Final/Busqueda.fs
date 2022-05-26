// Carlos Alfonso Hernandez Rojas  170170
// Examen Final
// Inteligencia artificial 1
namespace Busqueda

type nodo<'s,'a> = 
    {
        estado : 's
        profundidad : int
        costo_ruta : float
        padre : nodo<'s, 'a> option
        accion : 'a option
    }   //Estructura de datos para un nodo

type problema<'s, 'a> =
    {
        inicio : 's   //estado (polimorfico)
        sucesores : 's ->('a * 's) list
        meta : 's -> bool
        costo : 's -> 'a -> 's -> float
    }

type estrategia<'s,'a, 'd> = 
    {
        agregar : 'd -> nodo<'s,'a> -> 'd
        siguiente : 'd -> ( nodo<'s,'a> * 'd )option 
        inicializar : nodo<'s,'a> -> 'd
    }

module Capitulo3 = 
    let creaNodo problema =
        {
            estado = problema.inicio
            profundidad = 0
            costo_ruta = 0.0
            padre = None
            accion = None
        }
    
    let expand n problema =
        n.estado
        |> problema.sucesores
        |> List.map (fun (accion, estado) -> 
            {
                estado = estado
                profundidad = n.profundidad + 1
                costo_ruta = n.costo_ruta + problema.costo n.estado accion estado
                padre = Some n
                accion = Some accion
            })

    let busquedaArbol problema estrategia =
        let nodo_raiz = creaNodo problema
        let bolsa = estrategia.inicializar nodo_raiz

        let rec loop bolsa =
            match estrategia.siguiente bolsa with
            | None -> None
            | Some (n, bolsa) -> 
                if problema.meta n.estado
                then Some n
                else expand n problema
                     |> List.fold estrategia.agregar bolsa
                     |> loop
        loop bolsa      

    let rec acciones n = 
        match n.padre with
        | None -> []
        | Some m -> acciones m @ [Option.get n.accion]  

    let busquedaGrafo problema estrategia id=
        let nodo_raiz = creaNodo problema
        let bolsa = estrategia.inicializar nodo_raiz
        let agregarConDeteccion agregar procesados bolsa n =
            if Set.contains (id n) procesados
            then bolsa
            else agregar bolsa n
        let rec loop (bolsa, procesados) =
            match estrategia.siguiente bolsa with
            | None -> None
            | Some (n, bolsa) -> 
                if problema.meta n.estado
                then Some n
                else expand n problema
                    |> List.fold (agregarConDeteccion estrategia.agregar procesados) bolsa
                    |> (fun bolsa -> loop(bolsa,  Set.add (id n) procesados))
        loop (bolsa, Set.empty)    
