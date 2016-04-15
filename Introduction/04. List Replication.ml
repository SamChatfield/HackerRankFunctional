let rec repeat e n acc = match n with
    | 0 -> acc
    | n -> repeat e (n-1) (e::acc)

let f n arr =
    let rec aux n arr acc = match arr with
        | [] -> acc
        | a::arr -> aux n arr (acc @ repeat a n [])
    in aux n arr []

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans
;;