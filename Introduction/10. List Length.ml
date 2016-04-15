let len lst =
    let rec aux n = function
        | []    -> n
        | _::tl -> aux (n+1) tl
    in aux 0 lst

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let lst = read_lines() in
    let ans = len lst in
    print_int ans
;;