let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let split_lines text =
  List.filter (fun l -> l <> String.empty) (String.split_on_char '\n' text)

let sum = List.fold_left ( + ) 0
let sumf = List.fold_left ( +. ) 0.
let product = List.fold_left ( * ) 1
let productf = List.fold_left ( *. ) 1.

let solve year day part2 solver exampleResult =
  let prefix = string_of_int year ^ "/" ^ string_of_int day in
  let suffix = ".txt" in
  let exampleFile = prefix ^ (if part2 then "example2" else "example") ^ suffix in
  let inputFile = prefix ^ suffix in
  let eResult = solver part2 (read_whole_file exampleFile) in
  let realResult = solver part2 (read_whole_file inputFile) in
    Printf.printf "12/%d/%d%s example: %f \n" day year (if part2 then " part 2" else "") eResult;
    assert (eResult = exampleResult);
    Printf.printf "12/%d/%d%s: %f \n" day year (if part2 then " part 2" else "") realResult;;

let digits = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let day1 part2 input =
  let lines = split_lines input in
  let line_value line =
    let first_num = ref (-1.) in
    let last_num = ref (-1.) in
    let handle_num num = last_num := num; if !first_num = -1. then first_num := num in
    let proc_ch idx ch = match idx, ch with
                              | _, '0'..'9' -> let num = float_of_string (String.make 1 ch) in
                                handle_num num
                              | _ -> if part2 then
                                List.iteri (fun numMinusOne numAsString ->
                                  (* print_string line;
                                  print_newline();
                                  print_int idx;
                                  print_newline();
                                  print_int (String.length numAsString);
                                  print_newline(); *)
                                  try
                                    if (String.sub line idx (String.length numAsString)) = numAsString then
                                      let num = 1. +. (float_of_int numMinusOne) in
                                        handle_num num
                                    with _ -> ())
                                digits
                              else
                                ()
                              in
    String.iteri proc_ch line;
    (* Printf.printf "%s -> %d \n" line (!first_num * 10 + !last_num); *)
    if !first_num <> -1. then (!first_num *. 10. +. !last_num) else 0. in
  sumf (List.map line_value lines);;

solve 2023 1 false day1 142.;;
solve 2023 1 true day1 281.;;

let day2 part2 input =
  let lines = split_lines input in
  let process_line line =
    let id_idx = 5 in
    let id_end_idx = String.index line ':' in
    let id = float_of_string (String.sub line id_idx (id_end_idx - id_idx)) in
    let block_limit = [("red", 12.); ("green", 13.); ("blue", 14.)] in
    let block_min = ref [("red", 0.); ("green", 0.); ("blue", 0.)] in
    let sets = String.split_on_char ';' (String.sub line (2 + id_end_idx) ((String.length line) - id_end_idx - 2)) in
    let over_limit = ref false in
    (* for each set, check for an impossible amount of blocks *)
    List.iter (fun set ->
        let block_amounts = List.map String.trim (String.split_on_char ',' set) in
        let amounts = List.map (fun amt ->
                                    let parts = String.split_on_char ' ' amt in
                                      (float_of_string (List.nth parts 0), List.nth parts 1)) block_amounts in
          List.iter (fun amt -> match amt with
                        | num, color ->
                            if num > (List.assoc color block_limit) then over_limit := true;
                            if num > (List.assoc color !block_min) then block_min := (color, num) :: (List.remove_assoc color !block_min)) amounts
        ) sets;
    if part2 then (productf (List.map (fun p -> match p with _, amount -> amount) !block_min)) else if !over_limit then 0. else id
  in
  (sumf (List.map process_line lines));;

solve 2023 2 false day2 8.;;
solve 2023 2 true day2 2286.;;

let list2d input =
  let lines = split_lines input in
  List.map (fun line ->
    let chars = ref [] in
    String.iter
      (fun ch ->
        chars := !chars @ [ch])
      line;
    !chars)
    lines;;

let iter_grid f grid =
  List.iteri (fun y -> fun line -> List.iteri (fun x -> fun ch -> f x y ch) line) grid

let iter_adjacent f grid x y =
  for x1 = x - 1 to x + 1 do
    for y1 = y - 1 to y + 1 do
      if not (x1 < 0 || y1 < 0 || x1 >= List.length (List.nth grid 0) || y1 >= List.length grid || x1 = x && y1 = y) then
        f x1 y1 (List.nth (List.nth grid y1) x1)
    done
  done;;

let day3 part2 input =
  let nums = ref [] in
  let gears = ref [] in
  let current_num = ref "" in
  let current_num_x = ref (-1) in
  let current_num_y = ref (-1) in
  let grid = list2d input in
  (* Pad each row with . so numbers can't span a line break *)
  let padded_grid = List.map (fun l -> '.'::l @ ['.']) grid in
  let total = ref 0. in
  iter_grid (fun x -> fun y -> fun ch ->
    match ch with
    | '0'..'9' ->
      if !current_num <> String.empty then
        current_num := !current_num ^ String.make 1 ch
      else
        begin
          current_num := String.make 1 ch;
          current_num_x := x;
          current_num_y := y;
        end
    | _ ->
      if !current_num <> String.empty then
        nums := !nums @ [(!current_num, !current_num_x, !current_num_y)];
        current_num := ""
    ) padded_grid;
  List.iter (fun num_tuple ->
    match num_tuple with
      | (num, x, y) ->
        let has_adjacent_symbol = ref false in
        let has_adjacent_gears = ref [] in
          for x1 = x to x + (String.length num - 1) do
            iter_adjacent (fun adj_x -> fun adj_y -> fun ch ->
              match ch with
              | '0'..'9'|'.' -> ()
              | '*' ->
                begin
                has_adjacent_symbol := true;
                if not (List.exists (( = ) (adj_x, adj_y)) !has_adjacent_gears) then
                  has_adjacent_gears := (adj_x, adj_y) :: !has_adjacent_gears
                end
              | _ -> has_adjacent_symbol := true) padded_grid x1 y
          done;
          if !has_adjacent_symbol then total := !total +. float_of_string num;
          List.iter (fun gear_coords ->
            (match (List.assoc_opt gear_coords !gears) with
                |  Some numbers -> gears := (gear_coords, num :: numbers) :: List.remove_assoc (x,y) !gears
                |  None -> gears := (gear_coords, [num]) :: !gears)) !has_adjacent_gears
    ) !nums;
  if part2 then
    sumf (List.map (fun gear ->
      match gear with
      | ((x, y), [num1; num2]) -> begin
        print_int x; print_string ", "; print_int y; print_string ": "; print_string (num1 ^ " " ^ num2 ^ "\n");
        (float_of_string num1) *. (float_of_string num2)
      end
      | ((x, y), numbers) -> 0.) !gears)
  else
    !total;;


solve 2023 3 false day3 4361.;;
solve 2023 3 true day3 467835.;;