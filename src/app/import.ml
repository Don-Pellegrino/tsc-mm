open! Core

module Form = struct
  let of_filename filename =
    Csv.load filename
    |> Csv.to_array
    |> Array.to_sequence
    |> Fn.flip Sequence.drop 1
    |> Sequence.map ~f:(function
         | [| _ts; name; rank; ranking_up_down; comms |] ->
           Player.of_csv ~name ~rank ~ranking_up_down ~comms ~main_hero_pool:"" ~secondary_hero_pool:""
         | [| _ts; name; rank; ranking_up_down; main_hero_pool; secondary_hero_pool; comms; _att; _part |]
           ->
           Player.of_csv ~name ~rank ~ranking_up_down ~comms ~main_hero_pool ~secondary_hero_pool
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
         | [| "" |] -> None
         | [| name |] -> Some name
         | arr ->
           failwithf
             !"(%s) Invalid row format (%d) for %{sexp: string array}"
             filename (Array.length arr) arr () )
    |> Sequence.to_list
end
