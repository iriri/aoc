open Aoc
open Pervs

let distance (x, y, z) (x1, y1, z1) =
   let dx, dy, dz = x - x1, y - y1, z - z1
   (dx * dx) + (dy * dy) + (dz * dz)

type [<Struct>] Node = {
   mutable Parent : int
   mutable Size : int }

let rec find (forest : _[]) i =
   match forest[i].Parent with
   | j when j = i -> j
   | j ->
      forest[i].Parent <- find forest j
      forest[i].Parent

let union (forest : _[]) i j =
   let i, j = find forest i, find forest j
   if i = j then None else
      forest[j].Parent <- i
      forest[i].Size <- forest[i].Size + forest[j].Size
      Some i

let one (lines : Seq<String>) = monad {
   let boxes = lines |> Array.ofSeq |> traverse (fun s ->
      match (s.Split ',') |> Array.ofSeq |> traverse parse with
      | Some [|x; y; z|] -> Some (x, y, z)
      | _ -> None)
   let! boxes : _[] = boxes ||! lazy "invalid input"

   let distances =
      let rec go (acc : MVec<_>) i j =
         if i = length boxes then acc
         elif j = length boxes then go acc (i + 1) (i + 2) else
            go (acc |> MVec.add (distance boxes[i] boxes[j], i, j)) i (j + 1)
      go zero 0 1
   distances.Sort (ComparisonIdentity.FromFunction (fun (d, _, _) (d', _, _) -> d <=> d'))

   let forest = Array.init (length boxes) (fun i -> {Parent = i; Size = 1})
   distances |> Seq.take 1000 |> Seq.iter (fun (_, i, j) -> union forest i j |> ignore)
   let sizes = (Map.empty, forest) ||> fold (fun acc node ->
      acc |> Map.change node.Parent (function
         | Some n -> Some (max n node.Size)
         | None -> Some node.Size))
   let sizes = sizes |> Seq.map snd |> Array.ofSeq
   sizes |> Array.sortInPlaceWith ((<=>) >> ((<<) (~-)))
   printfn "%d" ((1, sizes |> Seq.take 3) ||> fold (*))
   return boxes, distances }

let two (boxes : _[], distances) =
   let forest = Array.init (length boxes) (fun i -> {Parent = i; Size = 1})
   let _ = (length boxes, distances) ||> loop (fun acc (_, i, j) ->
      match union forest i j with
      | Some _ when acc = 2 ->
         printfn "%d" (fst boxes[i] * fst boxes[j])
         0, false
      | Some _ -> acc - 1, true
      | _ -> acc, true)
   Ok ()

[<EntryPoint>]
let main args = run (one >=> two) args
