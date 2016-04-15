let filter_pos lst =
    let rec aux lst acc n = match lst, n with
        | [], _                      -> acc
        | hd::tl, _ when n mod 2 = 0 -> aux tl acc (n+1)
        | hd::tl, _                  -> aux tl (hd :: acc) (n+1)
    in List.rev (aux lst [] 0)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let lst = read_lines() in
    let ans = filter_pos lst in
    List.iter (fun x -> print_int x; print_newline ()) ans
;;