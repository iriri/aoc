open Aoc
open Pervs
open Microsoft.Z3 // Lol

type [<Struct>] Lights = Lights of u64

module Lights =
   let ofArray (xs : _[]) =
      if length xs > 64 then None else
         (u64 0, Seq.indexed xs)
         ||> fold (fun acc (i, x) -> if x then acc ||| (u64 1 <<< i) else acc)
         |> (Lights >> Some)

   let pushButton (button : _[]) x =
      (x, button) ||> fold (fun (Lights acc) i -> Lights <| acc ^^^ (u64 1 <<< i))

type Row = {
   Goal : Lights
   Buttons : int[][]
   Joltages : int[] }

let parse lines =
   let parseRow s : Result<Row, String> = monad {
      let expect i c s =
         s |> tryItem i >>= (fun c' -> if c' = c then Some () else None) ||! lazy "invalid input"
      let! _ = expect 0 '[' s
      let! i = s |> indexOf "]" ||! lazy "invalid input"
      let! goal = s[1..i - 1] |> String.toArray |> traverse (function
         | '.' -> Ok false
         | '#' -> Ok true
         | _ -> Error "invalid input")
      let! goal = Lights.ofArray goal ||! lazy "invalid input"
      let! _ = expect (i + 1) ' ' s

      let parseInts (s : String) = s.Split ',' |> Array.ofSeq |> traverse parse
      let! j = s |> indexOf " {" ||! lazy "invalid input"
      let! buttons = s[i + 2..j - 1].Split ' ' |> Array.ofSeq |> traverse (fun s -> monad {
         let! _ = expect 0 '(' s
         let! _ = expect (length s - 1) ')' s
         return! parseInts s[1..(length s - 2)] ||! lazy "invalid input" })

      let! _ = expect (length s - 1) '}' s
      let! joltages = parseInts s[j + 2..length s - 2] ||! lazy "invalid input"
      return {Goal = goal; Buttons = buttons; Joltages = joltages} }
   lines |> Array.ofSeq |> traverse parseRow

let one lines = monad {
   let rec bfs (queue : System.Collections.Generic.Queue<_>) visited row =
      if isEmpty queue then None else
         let lights, button, path = queue.Dequeue ()
         let lights = lights |> Lights.pushButton button
         if lights = row.Goal then Some (length path + 1)
         elif visited |> Set.contains lights then bfs queue visited row else
            let path = path |> Vec.add button
            row.Buttons |> iter (fun b -> queue.Enqueue (lights, b, path))
            bfs queue (visited |> Set.add lights) row

   let! rows = parse lines
   let n = (0, rows) ||> fold (fun acc row ->
      let queue =
         row.Buttons
         |> Seq.map (fun b -> Lights (u64 0), b, zero)
         |> System.Collections.Generic.Queue<_>
      let visited = Set.singleton (Lights (u64 0))
      acc + (bfs queue visited row ?| 0))
   printfn "%d" n
   return rows }

let two rows = monad {
   use ctx = new Context ()
   let solveRow row =
      let solver = ctx.MkOptimize ()
      let buttons =
         row.Buttons
         |> Seq.mapi (fun i _ ->
            let b = ctx.MkIntConst (sprintf "b%d" i)
            solver.Assert (ctx.MkGe (b, ctx.MkInt 0))
            b)
         |> Array.ofSeq
      let joltages =
         row.Joltages
         |> Seq.mapi (fun i x ->
            let j = ctx.MkIntConst (sprintf "j%d" i)
            solver.Assert (ctx.MkEq (j, ctx.MkInt x))
            j)
         |> Array.ofSeq

      let indices = (Map.empty, Seq.indexed row.Buttons) ||> fold (fun acc (i, b) ->
         (acc, b) ||> Array.fold (fun acc j ->
            acc |> Map.change j (function
               | Some is -> Some (is |> MVec.add i)
               | None -> Some (MVec.singleton i))))
      indices |> Map.iter (fun j b ->
         let sum = ctx.MkAdd (b |> Seq.map (fun i -> buttons[i] :> ArithExpr))
         solver.Assert (ctx.MkEq (joltages[j], sum)))
      solver.MkMinimize <| ctx.MkAdd (buttons |> Seq.map (fun b -> b :> ArithExpr)) |> ignore
      if solver.Check () <> Status.SATISFIABLE then Error "uhh" else
         let model = solver.Model
         Ok ((0, buttons) ||> fold (fun acc b -> acc + (model.Evaluate (b) :?> IntNum).Int))

   let! n = (Ok 0, rows) ||> loop (fun acc row ->
      let res = lift2 (+) acc (solveRow row)
      res, Result.isOk res)
   printfn "%d" n
   return () }

[<EntryPoint>]
let main args = run (one >=> two) args
