﻿open System
open SFML.Graphics
open SFML.Window
open SFML.System

let crea_griglia size = Array.Parallel.init (size*size-1) ( fun x -> ( x%size , x/size ) ) 

let life table size = 
    let ci_sara mappa elemento  =  
        let conta_vicini mappa elemento =  
            let vicini (r,c) = [| (r+1,c) ; (r-1,c) ; (r,c+1) ; (r,c-1) ; (r+1,c+1) ; (r-1,c+1) ; (r+1,c-1) ; (r-1,c-1) |]
            elemento 
                |> vicini
                |> Array.sumBy ( fun x -> match Array.tryFind ((=) x) mappa  with | None -> 0 | _ -> 1 )
        let resta_viva x = x = 2 || x = 3 
        let resuscita = x = 3
        match  Array.tryFind ( (=) elemento ) mappa     with 
            | Some _ -> elemento |> conta_vicini mappa |> resta_viva  
            | None   -> elemento |> conta_vicini mappa |> resuscita 
    crea_griglia size |> Array.filter ( ci_sara table )  

let crea_random size = 
    let rnd = System.Random()
    crea_griglia size |> Array.filter ( fun _ -> rnd.Next(2) = 1 )


[<EntryPoint>]
let main arg =
    let size = 50

    let v_size = 500u
    let v_cell_size = float32 v_size / float32 size

    let win = new RenderWindow( VideoMode(v_size,v_size),"Game of Life")
    win.Closed.AddHandler( fun s a ->  Environment.Exit 0 ) 
    win.SetFramerateLimit(4u)

    let cella = new RectangleShape(Vector2f(v_cell_size,v_cell_size)) 
    cella.FillColor <- Color.Green   

    let draw (x,y)= //set_cella cella>>(fun x -> win.Draw(x))
        cella.Position <- Vector2f(cella.Size.X * float32 x , cella.Size.Y * float32 y)
        win.Draw(cella)

    let rec main_loop t = 
        win.DispatchEvents()
        win.Clear()
        let tmp = life t size
        tmp |> Array.iter draw
        win.Display()
        main_loop tmp

    crea_random size |> main_loop
    0
