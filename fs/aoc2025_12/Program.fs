open Aoc
open Pervs

type Region = {
   Width : int
   Height : int
   Presents : int[] }

let parse (lines : Seq<String>) =
   lines |> Seq.skip 30 |> Array.ofSeq |> traverse (fun line -> monad {
      let! i = line |> indexOf "x"
      let! width = parse line[..i - 1]
      let! j = line |> indexOf ": "
      let! height = parse line[i + 1..j - 1]
      let! presents = line[j + 2..].Split ' ' |> Array.ofSeq |> traverse parse
      return {Width = width; Height = height; Presents = presents} })

let one lines = monad {
   let! regions = parse lines ||! lazy "invalid input"
   let n = (0, regions) ||> fold (fun acc region ->
      let regionArea = region.Width * region.Height
      let presentsArea = 3 * 3 * ((0, region.Presents) ||> fold (+))
      acc + if regionArea >= presentsArea then 1 else 0)
   printfn "%d" n
   return () }

[<EntryPoint>]
let main args = run one args
