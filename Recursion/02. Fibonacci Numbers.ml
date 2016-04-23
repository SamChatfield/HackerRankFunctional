let fib n =
    let rec aux lla la =
        function
        | 1                    -> lla + la
        | i when n - i + 1 = 1 -> aux 0 0 (i-1)
        | i when n - i + 1 = 2 -> aux 0 1 (i-1)
        | i                    -> aux la (lla + la) (i-1)
    in aux 0 0 n

let () =
    let n = read_int () in
    let ans = fib n in
    print_int ans
;;