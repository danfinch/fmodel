
namespace FModel

open System

type 'a Range = {
  Lower             : int
  Upper             : int
  Total             : int
  Items             : 'a list
}

type 'a Page = {
  Page              : int
  Size              : int
  Total             : int
  Items             : 'a list
} with
  member o.HasNext = o.Page < o.TotalPages
  member o.HasPrevious = o.Page > 1
  member o.TotalPages =
    let total = o.Total |> float
    let size = o.Size |> float
    let ceil = total / size |> Math.Ceiling |> int
    Math.Max (ceil, 1)
  member o.IsFirst = o.Page = 1
  member o.IsLast = o.Page = o.TotalPages

type 'a Store = {
  TryGet            : string -> 'a option
  Create            : string -> 'a -> unit
  Put               : string -> 'a -> unit
  Delete            : string -> unit
  Count             : unit -> int
  Range             : int -> int -> 'a Range
  Has               : string -> bool
  Many              : string list -> (string * 'a option) list
}

module Store =
  let put k v s = s.Put k v
  let tryGet k s = s.TryGet k
  let get k s =
    match s.TryGet k with
    | Some v -> v
    | _ -> failwith "Could not find value."
  let delete k s = s.Delete k
  let count s = s.Count ()
  let range l u s = s.Range l u
  let create k v s = s.Create k v
  let page p ps s =
    let l = (p - 1) * ps
    let r = s |> range l (l + ps - 1)
    { Page = p
      Size = ps
      Total = r.Total
      Items = r.Items
    }

