open Aoc
open Pervs

let one (lines : Seq<String>) = monad {
   let lines = Array.ofSeq lines
   let! top = lines |> tryItem 0 ||! lazy "invalid input"
   let! i = top |> indexOf "S" ||! lazy "invalid input"
   let r = ((0, Set.singleton i), lines |> Seq.skip 1) ||> foldWhile (fun (n, beams) row ->
      ((n, beams), beams) ||> foldWhile (fun (n, beams) i -> monad {
         match! row |> tryItem i with
         | '^' ->
            let beams = beams |> Set.remove i
            let beams = if i = 0 then beams else beams |> Set.add (i - 1)
            let beams = if i = length row - 1 then beams else beams |> Set.add (i + 1)
            return n + 1, beams
         | _ -> return n, beams }))
   let! n, _ = r ||! lazy "invalid input"
   printfn "%d" n
   return lines, i }

let two (lines, i) = monad {
   let table = (Map.ofArray [|(i, u64 1)|], lines |> Seq.skip 1) ||> fold (fun table row ->
      (table, table) ||> Map.fold (fun table i n ->
         match row |> item i with
         | '^' ->
            let table = table |> Map.remove i
            let add = function
               | None -> Some n
               | Some m -> Some (n + m)
            let table = if i = 0 then table else table |> Map.change (i - 1) add
            if i = length row - 1 then table else table |> Map.change (i + 1) add
         | _ -> table))
   printfn "%d" ((u64 0, table) ||> fold (+))
   () }

[<EntryPoint>]
let main args = run (one >=> two) args
