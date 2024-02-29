let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let split_lines text =
  String.split_on_char '\n' text

let solve year day solver exampleResult =
  let prefix = string_of_int year ^ "/" ^ string_of_int day in
  let suffix = ".txt" in
  let exampleFile = prefix ^ "example" ^ suffix in
  let inputFile = prefix ^ suffix in
  let eResult = solver (read_whole_file exampleFile) in
  let realResult = solver (read_whole_file inputFile) in
    assert (eResult = exampleResult);
    Printf.printf "12/%d/%d: %d \n" day year realResult;;

let day1 input =
  let lines = split_lines input in
  let line_value line =
    let first_num = ref (-1) in
    let last_num = ref (-1) in
    let proc_ch ch = match ch with
                              | '0'..'9' -> let num = int_of_string (String.make 1 ch) in
                                  last_num := num;
                                  if !first_num = -1 then first_num := num
                              | _ -> () in
    String.iter proc_ch line;
    (* Printf.printf "%s -> %d \n" line (!first_num * 10 + !last_num); *)
    if !first_num <> -1 then (!first_num * 10 + !last_num) else 0 in
  List.fold_left ( + ) 0 (List.map line_value lines);;

solve 2023 1 day1 142;;