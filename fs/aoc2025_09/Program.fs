open Aoc
open Pervs

let area (x, y) (x1, y1) =
   let w = abs (x - x1)
   let h = abs (y - y1)
   (w + i64 1) * (h + i64 1)

let one (lines : Seq<String>) = monad {
   let corners = lines |> Array.ofSeq |> traverse (fun s ->
      match (s.Split ',') |> Array.ofSeq |> traverse parse with
      | Some [|x; y|] -> Some (x, y)
      | _ -> None)
   let! corners : _[] = corners ||! lazy "invalid input"
   let largest =
      let rec go acc i j =
         if i = length corners then acc
         elif j = length corners then go acc (i + 1) (i + 2) else
            let acc = max acc (area corners[i] corners.[j])
            go acc i (j + 1)
      go 0 0 1
   printfn "%d" largest
   return corners }

type Direction = Left | Right | Straight

let direction (x, y) (x1, y1) (x2, y2) =
   let left = (x1 - x) * (y2 - y)
   let right = (x2 - x) * (y1 - y)
   if left > right then Left
   elif right > left then Right
   else Straight

let pointInRect (x, y) ((x', y'), (x1', y1')) =
   let left, right = if x' < x1' then x', x1' else x1', x'
   let low, high = if y' < y1' then y', y1' else y1', y'
   x >= left && x <= right && y >= low && y <= high

let pointInPoly (x, y) (vertices : (i64 * i64)[]) =
   let wind acc ((x', y'), (x1', y1')) =
      match direction (x', y') (x1', y1') (x, y) with
      | Left -> (if y1' <= y && y < y' then acc - 1 else acc), true
      | Right -> (if y' <= y && y < y1' then acc + 1 else acc), true
      | Straight -> if pointInRect (x, y) ((x', y'), (x1', y1')) then 1, false else acc, true
   if isEmpty vertices then false else
      let pairs =
         Seq.append (Seq.pairwise vertices) (Seq.singleton (Array.last vertices, head vertices))
      ((0, pairs) ||> Seq.loop wind) <> 0

let lineIntersectsPoly (p, q) (vertices : (i64 * i64)[]) =
   let intersects (p, q) (p1, q1) =
      let d = direction p q p1
      if d = Straight then false else
         let d1 = direction p q q1
         if d1 = Straight then false else
            let d2 = direction p1 q1 p
            if d2 = Straight then false else
               let d3 = direction p1 q1 q
               d3 <> Straight && d <> d1 && d2 <> d3
   if isEmpty vertices then false else
      let pairs =
         Seq.append (Seq.pairwise vertices) (Seq.singleton (Array.last vertices, head vertices))
      (false, pairs) ||> Seq.loop (fun _ (p', q') ->
         if intersects (p, q) (p', q') then true, false else false, true)

let two vertices =
   let rectInPoly (x, y) (x1, y1) =
      pointInPoly (x, y1) vertices
      && pointInPoly (x1, y) vertices
      && not (lineIntersectsPoly ((x, y), (x, y1)) vertices)
      && not (lineIntersectsPoly ((x, y), (x1, y)) vertices)
      && not (lineIntersectsPoly ((x1, y1), (x, y1)) vertices)
      && not (lineIntersectsPoly ((x1, y1), (x1, y)) vertices)
   let largest =
      let rec go acc i j =
         if i = length vertices then acc
         elif j = length vertices then go acc (i + 1) (i + 2) else
            let acc =
               if rectInPoly vertices[i] vertices[j] then max acc (area vertices[i] vertices.[j])
               else acc
            go acc i (j + 1)
      go 0 0 1
   printfn "%d" largest
   Ok ()

[<EntryPoint>]
let main args = run (one >=> two) args
