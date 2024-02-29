let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let split_lines text =
  String.split_on_char '\n' text

let solve year day part2 solver exampleResult =
  let prefix = string_of_int year ^ "/" ^ string_of_int day in
  let suffix = ".txt" in
  let exampleFile = prefix ^ (if part2 then "example2" else "example") ^ suffix in
  let inputFile = prefix ^ suffix in
  let eResult = solver part2 (read_whole_file exampleFile) in
  let realResult = solver part2 (read_whole_file inputFile) in
    assert (eResult = exampleResult);
    Printf.printf "12/%d/%d%s: %d \n" day year (if part2 then " part 2" else "") realResult;;

let digits = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let day1 part2 input =
  let lines = split_lines input in
  let line_value line =
    let first_num = ref (-1) in
    let last_num = ref (-1) in
    let handle_num num = last_num := num; if !first_num = -1 then first_num := num in
    let proc_ch idx ch = match idx, ch with
                              | _, '0'..'9' -> let num = int_of_string (String.make 1 ch) in
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
                                      let num = 1 + numMinusOne in
                                        handle_num num
                                    with _ -> ())
                                digits
                              else
                                ()
                              in
    String.iteri proc_ch line;
    (* Printf.printf "%s -> %d \n" line (!first_num * 10 + !last_num); *)
    if !first_num <> -1 then (!first_num * 10 + !last_num) else 0 in
  List.fold_left ( + ) 0 (List.map line_value lines);;

solve 2023 1 false day1 142;;
solve 2023 1 true day1 281;;