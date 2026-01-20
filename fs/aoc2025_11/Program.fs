open Aoc
open Pervs

let parse (lines : Seq<String>) =
   (Map.empty, lines) ||> foldWhile (fun paths line ->
      line |> indexOf ": " >>= fun i ->
         let edges = line[i + 2..].Split ' ' |> Array.ofSeq
         Some (paths |> Map.add line[..i - 1] edges))

let one lines = monad {
   let! paths = parse lines ||! lazy "invalid input"
   let rec dfs pos = monad {
      let! edges = paths |> Map.tryFind pos
      let branches = (0, edges) ||> foldWhile (fun acc edge ->
         if edge = "out" then Some (acc + 1)
         else dfs edge |>> (+) acc)
      return! branches }
   let! n = dfs "you" ||! lazy "invalid input"
   printfn "%d" n
   return paths }

let two paths = monad {
   let dfs f (dac, fft, pos) = monad {
      let! edges = paths |> Map.tryFind pos
      let dac, fft = dac || pos = "dac", fft || pos = "fft"
      let branches =
         (u64 0, edges) ||> foldWhile (fun acc edge ->
            if edge = "out" then Some (acc + (if dac && fft then u64 1 else u64 0))
            else f (dac, fft, edge) |>> (+) acc)
      return! branches }
   let! n = (memoizeFix dfs) (false, false, "svr") ||! lazy "invalid input"
   printfn "%d" n
   return () }

[<EntryPoint>]
let main args = run (one >=> two) args
