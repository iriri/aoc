let (>>) f g x = g (f x)

let (||>) (x, y) f = f x y

let flip f x y = f y x

let range i j = Seq.ints i |> Seq.take (j - i)

let adj = Array.to_seq [|
   (1, 0);
   (-1, 0);
   (0, 1);
   (0, -1);
   (1, 1);
   (-1, 1);
   (1, -1);
   (-1, -1);
|]

let rec one lines =
   let len_y = Array.length lines in
   let counts = range 0 len_y |> Seq.map (fun y ->
      let s = lines.(y) in
      let len_x = Bytes.length s in
      range 0 len_x
         |> Seq.filter (fun x -> adj
            |> Seq.filter (fun (x', y') ->
               let x' = x + x' in
               let y' = y + y' in
               Bytes.get lines.(y) x = '@' && (x' < 0
                  || x' >= len_x
                  || y' < 0
                  || y' >= len_y
                  || Bytes.get lines.(y') x' <> '@'))
            |> Seq.length
            |> (<) 4)
         |> Seq.length) in
   (0, counts) ||> Seq.fold_left (+) |> print_int;
   print_newline ()

let rec two lines =
   let remove lines =
      let len_y = Array.length lines in
      let counts = range 0 len_y |> Seq.map (fun y ->
         let len_x = Bytes.length lines.(y) in
         range 0 len_x |> Seq.filter (fun x ->
            let rem = adj
               |> Seq.filter (fun (x', y') ->
                  let x' = x + x' in
                  let y' = y + y' in
                  Bytes.get lines.(y) x = '@' && (x' < 0
                     || x' >= len_x
                     || y' < 0
                     || y' >= len_y
                     || Bytes.get lines.(y') x' = '.'))
               |> Seq.length
               |> (<) 4 in
            if rem then Bytes.set lines.(y) x 'x';
            rem)
         |> Seq.length) in
      let n = (0, counts) ||> Seq.fold_left (+) in
      range 0 len_y |> Seq.iter (fun y -> range 0 (Bytes.length lines.(y))
         |> Seq.filter (Bytes.get lines.(y) >> (=) 'x')
         |> Seq.iter @@ flip (Bytes.set lines.(y)) '.');
      n in
   let rec go acc = match remove lines with
      | 0 -> acc
      | n -> go (acc + n) in
   go 0 |> print_int;
   print_newline ()


let () =
   if Array.length Sys.argv <> 2 then (
      prerr_endline "invalid arguments";
      exit 1);
   let lines = In_channel.with_open_text Sys.argv.(1) In_channel.input_lines in
   let lines = lines
      |> List.to_seq
      |> Seq.map Bytes.of_string
      |> Array.of_seq in
   one lines;
   two lines
