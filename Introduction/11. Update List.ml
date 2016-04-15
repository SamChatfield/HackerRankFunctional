let replace_abs lst =
    let rec aux acc = function
        | []    -> acc
        | n::tl -> aux (abs(n)::acc) tl
    in List.rev (aux [] lst)

let rec read_lines () = 
    try let line = read_line () in
        line :: read_lines()
    with
        End_of_file -> []

let () =
    let inp = read_lines () in
    let lst = List.map int_of_string inp in
    let result = replace_abs lst in
    let output = List.map string_of_int result in
    print_string (String.concat "\n" output)
;;