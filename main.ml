let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let split_lines text =
  split_on_char '\n' text

let solve year day solver exampleResult =
  let prefix = string_of_int year ^ "/" ^ string_of_int day in
  let suffix = ".txt" in
  let exampleFile = prefix ^ "example" ^ suffix in
  let inputFile = prefix ^ suffix in
  let eResult = solver (read_whole_file exampleFile) in
  let realResult = solver (read_whole_file inputFile) in
    assert (eResult = exampleResult);
    Printf.printf "%d" realResult;;

let day1 input =
  let lines = split_lines input in
  let line_value line 


solve 2024 1 day1 142;;