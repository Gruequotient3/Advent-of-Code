let filePath = "lib/data.txt"


let rec string_int (ls : string list) : int list =
    match ls with
    |[] -> []
    |x::xs -> (int_of_string x) :: string_int(xs);;


let read_file (path : string) =
    let ic = open_in path in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []
