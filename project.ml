let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan

module List = struct
    include List

    let int_list l = List.map int_of_string l

    let sum l =
        let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
        sum' 0 l

    let lines = String.split_on_char '\n'
end

module type Solver = sig
    val naloga1 : string -> string

    val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
    let cost_fun x = (x / 3) - 2

    let rec full_cost x =
        let c_cost = cost_fun x in
        if c_cost <= 0 then 0 else c_cost + full_cost c_cost

    let naloga1 data =
        let lines = List.lines data in
        lines |> List.int_list
        |> List.fold_left (fun s x -> s + cost_fun x) 0
        |> string_of_int

    let naloga2 data _part1 =
        data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
        |> string_of_int
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct

    let bigger h t =
        if h < List.hd t then 1 else 0

    let rec count int_list = 
        match int_list with
        | [] -> 0
        | [_] -> 0
        | x :: xs -> count xs + bigger x xs

    let rec vsota xs =
        match xs with
        | [] -> 0
        | x :: xs -> x + vsota xs

    let rec firstk k xs = 
        match xs with
        | [] -> []
        | x :: xs -> if k=1 then [x] else x :: firstk (k-1) xs
    
    let bigger_trojka glava rep = 
        if glava + vsota (firstk 2 rep) < vsota (firstk 3 rep) then 1 else 0
    
    let rec count_trojke int_list = 
        match int_list with
        | [] -> 0
        | glava :: rep -> (bigger_trojka glava rep) + (count_trojke rep)
        
    let naloga1 data = 
        let lines = List.lines data in
        lines |> List.int_list
        |> count
        |> string_of_int

    let naloga2 data _part1 = 
        let lines = List.lines data in
        lines |> List.int_list
        |> count_trojke
        |> string_of_int
end

module Solver2 : Solver = struct

    let vrednost_depth line = 
        if Char.escaped(String.get line 0) = "u" then - int_of_string(Char.escaped(String.get line 3))
        else if Char.escaped(String.get line 0) = "d" then int_of_string(Char.escaped(String.get line 5))
        else 0

    let rec vsota_depth sez =
        match sez with
        | [] -> 0
        | x :: xs -> vrednost_depth x + vsota_depth xs

    let vrednost_horz line = 
        if Char.escaped(String.get line 0) = "f" then int_of_string(Char.escaped(String.get line 8))
        else 0

    let rec vsota_horz sez =
        match sez with
        | [] -> 0
        | x :: xs -> vrednost_horz x + vsota_horz xs

    let rec usmerjena_vsota sez dir =
        match sez with
        | [] -> 0
        | x :: xs -> (vrednost_horz x) * (dir) + (usmerjena_vsota xs (dir + vrednost_depth x))

    let naloga1 data = 
        let lines = List.lines data in
        string_of_int((vsota_depth lines) * (vsota_horz lines))

    let naloga2 data _part1 = 
        let lines = List.lines data in
        string_of_int((usmerjena_vsota lines 0) * (vsota_horz lines))        
end
(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
    | "1" -> (module Solver1)
    | "2" -> (module Solver2)
    | _ -> failwith "Ni še rešeno"

let main () =
    let day = "2" in
    print_endline ("Solving DAY: " ^ day);
    let (module Solver) = choose_solver day in
    let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
    let p1_start = Sys.time () in
    let part1 = Solver.naloga1 input_data in
    let t1_time = Sys.time () -. p1_start in
    print_endline "PART 1:";
    print_endline part1;
    print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
    let p2_start = Sys.time () in
    let part2 = Solver.naloga2 input_data part1 in
    let t2_time = Sys.time () -. p2_start in
    print_endline "PART 2:";
    print_endline part2;
    print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
    izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
    izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
    ()

let _ = main ()