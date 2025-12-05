let (||>) (x, y) f = f x y

let int_of_char c = (Char.code c) - (Char.code '0')

let one lines =
   let biggest acc s =
      let (prev, curr, next) = ((0, 0, None), s)
         ||> String.fold_left (fun (prev, curr, next) c ->
            let x = int_of_char c in
            if x > curr then (curr, x, None) else match next with
               | Some n when n >= x -> (prev, curr, Some n)
               | _ -> (prev, curr, Some x)) in
      let n = match next with
         | Some n -> (curr * 10) + n
         | None -> (prev * 10) + curr in
      acc + n in
   (0, lines) ||> List.fold_left biggest |> print_int;
   print_newline ()

let two lines =
   let maxi i j s =
      let ss = Seq.ints (i + 1) |> Seq.take (j - i - 1) in
      ((i, s.[i]), ss) ||> Seq.fold_left (fun (i, max) j ->
         if s.[j] > max then (j, s.[j]) else (i, max)) in
   let biggest acc s =
      let len = String.length s in
      let rec go acc i rem =
         let (i, c) = maxi i (len - rem) s in
         let acc = (acc * 10) + (int_of_char c) in
         if rem = 0 then (acc, i) else go acc (i + 1) (rem - 1) in
      acc + fst (go 0 0 11) in
   (0, lines) ||> List.fold_left biggest |> print_int;
   print_newline ()

let () =
   if Array.length Sys.argv <> 2 then (
      prerr_endline "invalid arguments";
      exit 1);
   let lines = In_channel.with_open_text Sys.argv.(1) In_channel.input_lines in
   one lines;
   two lines
