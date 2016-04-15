let rev lst =
    let rec aux acc = function
        | []     -> acc
        | hd::tl -> aux (hd::acc) tl
    in aux [] lst

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let lst = read_lines() in
    let ans = rev lst in
    List.iter (fun x -> print_int x; print_newline ()) ans
;;