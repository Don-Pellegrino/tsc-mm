open! Core

module Form = struct
  let of_filename filename =
    Csv.load filename
    |> Csv.to_array
    |> Array.to_sequence
    |> Fn.flip Sequence.drop 1
    |> Sequence.map ~f:(function
         | [| _ts; name; practice; comms |] ->
           name, Player.of_csv ~name ~practice ~comms ~main_hero_pool:"" ~secondary_hero_pool:""
         | [| _ts; name; practice; main_hero_pool; secondary_hero_pool; comms; _att; _part |] ->
           name, Player.of_csv ~name ~practice ~comms ~main_hero_pool ~secondary_hero_pool
         | arr ->
           failwithf
             !"(%s) Invalid row format (%d) for %{sexp: string array}"
             filename (Array.length arr) arr () )
    |> Sequence.to_array
end

module Players = struct
  let of_filename filename =
    Csv.load filename
    |> Csv.to_array
    |> Array.to_sequence
    |> Sequence.filter_map ~f:(function
         | [| "" |]
          |[| "#" |] ->
           None
         | [| rank; _ |] when String.is_prefix rank ~prefix:"#" -> None
         | [| rank; name |] -> Some (Rank.of_csv rank, name)
         | arr ->
           failwithf
             !"(%s) Invalid row format (%d) for %{sexp: string array}"
             filename (Array.length arr) arr () )
    |> Sequence.to_list
end
