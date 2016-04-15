let fact n =
    let rec aux ans = function
        | 1 -> ans
        | n -> aux (ans * n) (n-1)
    in aux 1 n
        
let e x =
    let rec aux ans = function
        | 0 -> ans +. 1.
        | 1 -> aux (ans +. x) 0
        | n ->
            let term = (x ** float n) /. (float (fact n)) in
            aux (ans +. term) (n-1)
    in aux 0. 9

let e_lst lst =
    let rec aux acc = function
        | []     -> acc
        | hd::tl -> aux ((e hd)::acc) tl
    in List.rev (aux [] lst)

let rec read_lines () =
    try let line = read_line () in
        float_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let n::lst = read_lines() in
    let ans = e_lst lst in
    List.iter (fun x -> print_float x; print_newline ()) ans
;;