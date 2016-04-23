(* Only valid for y > x as in the problem *)

let rec gcd x y =
    let q = int_of_float (floor (float y /. float x)) in
    let r = y - (x * q) in
    if r = 0 then x else gcd r x

let line_to_intlist s =
    let rec aux acc =
        function
        | []     -> acc
        | hd::tl -> aux ((int_of_string hd) :: acc) tl
    in List.rev (aux [] (Str.split (Str.regexp " ") s))

let () =
    let xy = read_line () in
    let x = List.hd (line_to_intlist xy) in
    let y = List.hd (List.tl (line_to_intlist xy)) in
    let ans = gcd x y in
    print_int ans
;;