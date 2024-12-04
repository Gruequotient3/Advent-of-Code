
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


let rec remove_all_elem (l : 'a list) (v : 'a) =
    match l with
    |[] -> []
    |x::xs when (x = v) -> remove_all_elem xs v
    |x::xs -> x::(remove_all_elem xs v);;

let rec remove_first_elem (l : 'a list) (v : 'a) =
    match l with
    |[] -> []
    |x::xs when (x = v) -> xs
    |x::xs -> x::(remove_first_elem xs v);;


let get_data (path : string) =
    let lines = read_file path in
    let rec get_num_list (l : string list) =
        match l with
        |[] -> []
        |x::xs -> let split = String.split_on_char ' ' x in
            (remove_all_elem split "") :: get_num_list xs
    in get_num_list lines;;

let get_column (path : string) (col : int) =
    let data = get_data path in
    let rec get_list (l : string list list) (col : int) =
        match l with
        |[] -> []
        |x::xs -> match col with
                    |0 -> (List.nth x 0) :: (get_list xs col)
                    |_ -> (List.nth x 1) :: (get_list xs col)
    in get_list data col

let get_int_column_data (path : string) (col : int) =
    string_int (get_column path col);;


let rec get_min (l : int list) =
    match l with
    |[] -> -1
    |x::[] -> x
    |x::xs -> let min = get_min xs in
                if x < min then x
                else min

let get_distance (n1 : int) (n2 : int) =
    abs (n2 - n1);;

let rec get_simularity (l : int list) (v : int) =
    match l with
    |[] -> 0
    |x::xs when (x = v) -> 1 + get_simularity xs v
    |_::xs -> get_simularity xs v;;


let calc_distance (path : string) =
    let c0 = get_int_column_data path 0 in
    let c1 = get_int_column_data path 1 in
    let rec get_sum_distance (l0 : int list) (l1 : int list) =
        match l0, l1 with
        |[], [] -> 0
        |[], _ -> 0
        |_, [] -> 0
        |_, _ ->
            let min0 = get_min l0 in let l00 = remove_first_elem l0 min0 in
            let min1 = get_min l1 in let l01 = remove_first_elem l1 min1 in
            (get_distance min0 min1) + (get_sum_distance l00 l01)
        in get_sum_distance c0 c1;;


let calc_simularity (path : string) =
    let c0 = get_int_column_data path 0 in
    let c1 = get_int_column_data path 1 in
    let rec get_sum_simularity (l0 : int list) (l1 : int list) =
        match l0 with
        |[] -> 0
        |x::xs -> x * (get_simularity l1 x) + get_sum_simularity xs l1
    in get_sum_simularity c0 c1;;
