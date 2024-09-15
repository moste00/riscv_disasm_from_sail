let binary_chars_to_hex_digit b3 b2 b1 b0 =
  match (b3, b2, b1, b0) with
  | '0', '0', '0', '0' -> "0"
  | '0', '0', '0', '1' -> "1"
  | '0', '0', '1', '0' -> "2"
  | '0', '0', '1', '1' -> "3"
  | '0', '1', '0', '0' -> "4"
  | '0', '1', '0', '1' -> "5"
  | '0', '1', '1', '0' -> "6"
  | '0', '1', '1', '1' -> "7"
  | '1', '0', '0', '0' -> "8"
  | '1', '0', '0', '1' -> "9"
  | '1', '0', '1', '0' -> "A"
  | '1', '0', '1', '1' -> "B"
  | '1', '1', '0', '0' -> "C"
  | '1', '1', '0', '1' -> "D"
  | '1', '1', '1', '0' -> "E"
  | '1', '1', '1', '1' -> "F"
  | _ -> failwith "UNREACHABLE"

let binary_str_to_hex_str s =
  let slen = String.length s in
  let slen_mod4 = slen mod 4 in
  let padding =
    if slen_mod4 = 0 then ""
    else List.init (4 - slen_mod4) (fun _ -> "0") |> String.concat ""
  in
  let s_padded_4 = padding ^ s in
  let padlen = String.length s_padded_4 in
  let hexstr = Buffer.create (padlen / 4) in
  let i = ref 0 in
  while !i < padlen do
    let hex_digit =
      binary_chars_to_hex_digit s_padded_4.[!i]
        s_padded_4.[!i + 1]
        s_padded_4.[!i + 2]
        s_padded_4.[!i + 3]
    in
    Buffer.add_string hexstr hex_digit;
    i := !i + 4
  done;
  Buffer.contents hexstr

let str_starts_with prefix str =
  let plen = String.length prefix in
  let slen = String.length str in
  if plen > slen then false
  else (
    let str_prefix = String.sub str 0 plen in
    str_prefix = prefix
  )

let add_prefix_unless_exists prefix str =
  if str_starts_with prefix str then str else prefix ^ str

let get_some_or_failwith opt msg =
  match opt with Some thing -> thing | None -> failwith msg
