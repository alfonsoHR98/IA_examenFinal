// Carlos Alfonso Hernandez Rojas  170170
// Examen Final
// Inteligencia artificial 1
namespace Busqueda

module RutaSG =
    open Capitulo3

    type estado = int * int
    type accion =
        | Arriba
        | Abajo
        | Izquierda
        | Derecha
    
    let estado_inicial = (3,2) //Posicon de S desde esquina inferior izquierda
    let meta x = (x = (2,5)) //Posicion de G
    let costo _ _ _= 1.0
    let cuadros = Set[(2,2);(2,4);(1,3);(3,4);(4,4);(5,4);(6,4);(6,2)]//Definir las posiciones de los recuadros negros
    let sucesores ((x,y) : estado)= 
        [
            (Arriba, (x,y+1))
            (Abajo, (x,y-1))
            (Izquierda, (x-1,y))
            (Derecha, (x+1,y))
        ]
        |>List.filter(fun (_,pos) -> not(Set.contains pos cuadros)) //Excluir los cuadros negros en los sucesores

    let problema =
        {
            inicio = estado_inicial
            sucesores = sucesores
            meta = meta
            costo= costo
        }
     
    let estadoAlista (x1,x2) = [x1;x2]
    let Listameta =(estadoAlista (2,5))

    let h n=
        List.map2(fun x m -> if x = m  
                             then 0.0
                             else 1.0)  
                   (estadoAlista n.estado)
                   Listameta
        |> List.sum      

