open! Core

let of_filename filename =
  Csv.load filename
  |> Csv.to_array
  |> Array.to_sequence
  |> Fn.flip Sequence.drop 1
  |> Sequence.map ~f:(function
       | [|
           _ts;
           name;
           rank;
           practicing;
           queueing;
           main_hero_pool;
           secondary_hero_pool;
           pressure;
           _att;
           _part;
         |] ->
         Player.of_csv ~name ~rank ~practicing ~queueing ~pressure ~main_hero_pool ~secondary_hero_pool
       | arr -> failwithf !"Invalid row format (%d) for %{sexp: string array}" (Array.length arr) arr () )
  |> Sequence.to_array
