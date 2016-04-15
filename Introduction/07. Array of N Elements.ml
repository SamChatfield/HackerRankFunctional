(* returns an array of n elements *)
let make_list n =
    let rec aux acc = function
        | 0 -> acc
        | n ->
            let rn = Random.int 100 in
            aux (rn::acc) (n-1)
    in aux [] n

let () =
    let len = read_int () in
    let lst = make_list len in
    print_int (List.length lst)
;;