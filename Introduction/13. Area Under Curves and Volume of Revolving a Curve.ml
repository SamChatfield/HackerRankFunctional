open List

let pi = 4. *. atan 1.

let f x a b =
    let rec aux res a b =
        match a, b with
        | [], []             -> res
        | ahd::atl, bhd::btl -> aux (res +. float ahd *. x ** float bhd) atl btl
        | _, _               -> failwith "f - aux"
    in aux 0. a b

let curve_area l r a b =
    let n = int_of_float (float (r - l) /. 0.001) in
    let rec aux res =
        function
        | -1 -> res *. 0.001
        | i  ->
            let fi = f (float l +. 0.001 *. float i) a b in
            aux (res +. fi) (i-1)
    in aux 0. n
    
let curve_vol l r a b =
    let n = int_of_float (float (r - l) /. 0.001) in
    let rec aux res =
        function
        | -1 -> res *. 0.001
        | i  ->
            let r = f (float l +. 0.001 *. float i) a b in
            let r_sqr = r ** 2. in
            aux (res +. pi *. r_sqr) (i-1)
    in aux 0. n

let solve l r a b = [curve_area l r a b; curve_vol l r a b]

let rec read_lines () =
    try let line = read_line () in
        line :: read_lines ()
    with
        End_of_file -> []

let line_to_intlist s =
    let rec aux acc =
        function
        | []     -> acc
        | hd::tl -> aux ((int_of_string hd) :: acc) tl
    in rev (aux [] (Str.split (Str.regexp " ") s))

let () =
    let lines = read_lines () in
    let a = line_to_intlist (hd lines) in
    let b = line_to_intlist (hd (tl lines)) in
    let lr = line_to_intlist (hd (tl (tl lines))) in
    let l = hd lr in
    let r = hd (tl lr) in
    let ans = solve l r a b in
    iter (fun x -> print_float x; print_newline ()) ans
;;