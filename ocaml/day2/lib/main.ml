let filePath = "lib/data.txt"


type report =
    |Safe of int list * int * int
    |Unsafe of int list * int * int

module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end


module ListMonad : Monad with type 'a t = 'a list = struct
    type 'a t  = 'a list
    let return x = [x]
    let (>>=) x f = List.concat (List.map f x)
end


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

let split_data (str : string) =
    String.split_on_char ' ' str


let return = ListMonad.return
let (>>=) = ListMonad.(>>=)
let (let*) x f = x >>= f


let string_to_int (l : string list) =
    let* lst = l in
    return (int_of_string lst);;

let get_int_data (path : string) =
    let* string_data = read_file path in
    let* split_string_data = return(split_data string_data) in
    let* int_data = return (string_to_int split_string_data) in
    return int_data

let rec get_max_gap (l : int list) =
    match l with
        |[] -> -1
        |x::[] -> 0
        |x::y::xs -> let gap = abs (y-x) in
                    let max = get_max_gap (y::xs) in
                        if gap > max then gap
                        else max;;

let rec check_direction (l : int list) =
    match l with
    |[] -> 0
    |x::[] -> 0
    |x::y::xs -> let dir = if (y-x) = 0 then -2
                            else if (y-x) > 0 then 1
                            else -1
                in
                let last_dir = check_direction (y::xs) in
                if last_dir = 0 then dir
                else if last_dir != -2 && dir == last_dir then dir
                else -2


let set_security (l : int list list) =
    let* lst = l in
    let f x =   let gap = get_max_gap x in
                let dir = check_direction x in
                if gap <= 3 && dir != -2 then Safe (lst, gap, dir)
                else Unsafe (lst, gap, dir) in
    return (f lst);;


let count_safe (rep : report list) =
    List.fold_left (fun x y -> match y with |Safe _ -> 1 + x |Unsafe _ -> x) 0 rep


let nb_safe1 path = (count_safe (set_security (get_int_data path)));;

