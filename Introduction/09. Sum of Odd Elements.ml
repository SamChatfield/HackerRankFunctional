let sum_odd lst =
    let rec aux sum = function
        | []                           -> sum
        | n::tl when abs (n mod 2) = 1 -> aux (sum+n) tl
        | _::tl                        -> aux sum tl
    in aux 0 lst

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let lst = read_lines() in
    let ans = sum_odd lst in
    print_int ans
;;