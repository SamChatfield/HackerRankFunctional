let f n =
    let rec aux n acc = match n with
        | 0                       -> print_string (String.concat "\n" acc)
        | n                       -> aux (n-1) ("Hello World"::acc)
    in aux n []
    
let () =
    let n = read_int () in
    f n
;;