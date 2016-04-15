(* Delimeter d, List lst *)
let filter d lst =
    let rec aux acc = function
        | []                 -> acc
        | hd::tl when hd < d -> aux (hd::acc) tl
        | hd::tl             -> aux acc tl
    in List.rev (aux [] lst)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let d::lst = read_lines() in
    let ans = filter d lst in
    List.iter (fun x -> print_int x; print_newline ()) ans
;;