let mingle p q =
    let rec aux p q acc =
        match p, q with
        | [], []         -> acc
        | ph::pt, qh::qt -> aux pt qt (qh::ph::acc)
    in String.concat "" (List.rev (aux (Str.split (Str.regexp "") p) (Str.split (Str.regexp "") q) []))

let rec read_lines () =
    try let line = read_line () in
        line :: read_lines ()
    with
        End_of_file -> []

let () =
    let lines = read_lines () in
    let p = List.hd lines in
    let q = List.hd (List.tl lines) in
    let ans = mingle p q in
    print_string ans
;;