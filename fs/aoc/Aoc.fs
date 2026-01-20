module Aoc

open Pervs

let inline (~-) x = Checked.(~-) x
let inline (+) a b = Checked.(+) a b
let inline (-) a b = Checked.(-) a b
let inline (*) a b = Checked.(*) a b

let inline parse s =
   let mutable x = Unchecked.defaultof<_>
   match (^a : (static member TryParse : _ * byref<^a> -> bool) s, &x) with
   | true -> Some x
   | false -> None

let protect f x = try Ok <| f x with e -> Error e.Message

let slurp = protect System.IO.File.ReadLines

let indexOf (ss : String) (s : String) =
   match s.IndexOf ss with
   | -1 -> None
   | i -> Some i

let memoize f =
   let memo = HashMap ()
   fun x ->
      match memo.TryGetValue x with
      | true, x -> x
      | _ ->
         let y = f x
         memo.Add (x, y)
         y

let memoizeFix f x =
   let rec memoizeFix' f (memo : HashMap<_, _>) x =
      match memo.TryGetValue x with
      | true, x -> x
      | _ ->
         let y = f (memoizeFix' f memo) x
         memo.Add (x, y)
         y
   memoizeFix' f (HashMap ()) x

let run f args =
   match args with
   | [|arg|] ->
      match arg |> slurp >>= f with
      | Ok () -> 0
      | Error e ->
         eprintfn "%s" e
         1
   | _ ->
      eprintfn "invalid arguments"
      1
