let fact n =
    let rec aux ans =
        function
        | 0 -> 1
        | 1 -> ans
        | n -> aux (ans * n) (n-1)
    in aux 1 n

let pascal_point r c = fact r / (fact c * fact (r - c))

let make_row r =
    let rec aux acc =
        function
        | -1 -> acc
        | n  -> aux ((pascal_point r n)::acc) (n-1)
    in aux [] r

let pascal k =
    let rec aux acc =
        function
        | -1 -> acc
        | n  -> aux ((make_row n)::acc) (n-1)
    in aux [] (k-1)

let print_ans intlstlst =
    List.iter (fun intlst ->
        List.iter (fun x ->
            (print_int x; print_string " "))
        intlst; print_newline ())
    intlstlst

let () =
    let k = read_int () in
    let ans = pascal k in
    print_ans ans
;;