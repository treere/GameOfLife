// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
open System

let stampa_matrice t size =
    Console.Clear()
    printf "%A\n\n" 
        <| Array2D.init size size ( fun x y -> 
            match List.tryFind ((=) (x,y)) t with 
                | None -> ' ' 
                | _ -> '*'  )
    System.Threading.Thread.Sleep(1000)
    //Console.ReadKey() |> ignore

let rec life table size max_iter stampa = 
    
    let elimina_superflui mappa elemento  =  

        let conta_vicini mappa elemento =  

            let vicini (r,c) = [ (r+1,c) ; (r-1,c) ; (r,c+1) ; (r,c-1) ; (r+1,c+1) ; (r-1,c+1) ; (r+1,c-1) ; (r-1,c-1) ]

            vicini elemento 
                |> List.map ( fun x -> List.tryFind ( (=) x ) mappa )
                |> List.sumBy ( fun x -> match x with | None -> 0 | _ -> 1 ) 
        
        let resta_viva = function
            | 2 | 3 -> true
            | _ -> false 

        let resuscita = function
            | 3 -> true
            | _ -> false

        let n = conta_vicini mappa elemento 

        match  List.tryFind ( (=) elemento ) mappa     with 
            | Some _ -> resta_viva n 
            | None   -> resuscita n

    let temp = List.init (size*size-1) ( fun x -> ( x%size , x/size ) ) 
                |> List.filter ( elimina_superflui  table )  
    do stampa temp size
    match max_iter > 0 with
        | true ->  life temp size (max_iter-1) stampa
        | false -> temp

let crea_random size = 
    let rnd = System.Random()
    List.init ( size*size-1 ) (fun x -> ( x%size, x/size ) ) 
        |> List.filter ( fun _ -> rnd.Next(2) = 1 )

[<EntryPoint>]
let main arg =
    let size = 10
    let table = crea_random size //[(1,1);(1,2);(1,3)]
    //tabella size ripetizioni modo_stampa
    let _ = life table size 100 stampa_matrice
    0

(*let stampa_schermo ( form : Form) moltiplicatore  t size =
    form.Paint.Add(fun draw -> draw.Graphics.FillRectangle(Brushes.Black,0,0,moltiplicatore,moltiplicatore))
    t |>  List.iter 
            ( fun (x,y)  -> 
                form.Paint.Add(fun draw -> draw.Graphics.FillRectangle(Brushes.Black,x*moltiplicatore,y*moltiplicatore,moltiplicatore,moltiplicatore)))  
    System.Threading.Thread.Sleep(1000)
*)

(*
let table = [(1,1);(1,2);(1,3)]
let size = 10
let moltiplicatore = 30
let form = new Form(
            Height = size*moltiplicatore,
            Visible = true,
            Text = "My form"
            ) 
let stampa t (size : int ) = stampa_schermo form moltiplicatore t size 
//Application.Run(form)
*)